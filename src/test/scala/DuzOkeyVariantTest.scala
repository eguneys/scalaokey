package okey

import okey.format.{ Visual }

import Piece._

import org.specs2.matcher.Matcher

class DuzOkeyVariantTest extends OkeyTest {

  "Duz okey" should {

    val player = Player(EastSide)
    val player2 = Player(EastSide, drawMiddle = true)

    def beDuzOkey(visual: String): Matcher[Valid[Game]] = beSuccess.like {
      case g => g.table.visual must_== (Visual <<?(okey.variant.DuzOkey, visual)).visual
    }

    "position pieces correctly" in {
      val table = makeDuzOkey

      "each player gets 14 pieces" in {
        table.boards(EastSide).size must_== 15
        table.boards(WestSide).size must_== 14
        table.boards(NorthSide).size must_== 14
        table.boards(SouthSide).size must_== 14
      }

      "turn player gets an extra piece" in {
        val northTable = makeDuzOkey(NorthSide)

        northTable.boards(NorthSide).size must_== 15
        northTable.boards(EastSide).size must_== 14
        northTable.boards(WestSide).size must_== 14
        northTable.boards(SouthSide).size must_== 14
      }

      "dont have opener" in {
        table.opener must beNone
      }

      "have a sign piece and an okey piece" in {
        (table.sign up) must_== table.okey
      }

      "rest of the pieces are in middle" in {
        table.middles.size must_== 106 - (14 * 4 + 2)

        table must haveAllPieces
      }
    }
    

    "allow moves" in {

      "allow a piece to be drawn middle" in {
        makeDuzOkeyGame("""
r13
r1r2r3
b1
""", player).playMoves(EastSide, DrawMiddle) must beDuzOkey("""
r13
r2r3
b1r1
""")
      }

      "allow a piece to be drawn left" in {
        makeDuzOkeyGame("""
r13
r1r2r3
b1






l10
""", player).playMoves(EastSide, DrawLeft) must beDuzOkey("""
r13
r1r2r3
b1l10
""")
      }

      "allow a piece to be discarded after drawn left" in {
        makeDuzOkeyGame("""
r13
r1r2r3
b1






l1
""", player).playMoves(EastSide, DrawLeft, Discard(B1)) must beDuzOkey("""
r13
r1r2r3
l1



b1
""")
      }

      "allow a piece to be discarded" in {
        makeDuzOkeyGame("""
r13
r1r2r3
b1
""", player).playMoves(EastSide, DrawMiddle, Discard(B1)) must beDuzOkey("""
r13
r2r3
r1



b1
""")
      }

      "dont allow pieces to be opened series" in {
        makeDuzOkeyGame("""
r13

r4l4g4b4r4r5r6r7r13r13
""", player2).playMoves(EastSide, OpenSeries(Piece.<>(4))) must beFailure
      }


      "dont allow pieces to be opened pairs" in {
        makeDuzOkeyGame("""
r13

r4r5r6r7g7g7g8g8g9g9
""", player2).playMoves(EastSide, OpenPairs(G8.w)) must beFailure
      }

      "dont allow drawn piece to be left" in {
        val player3 = Player(side = EastSide, drawLeft = Some(Piece.B2))
        makeDuzOkeyGame("""
r13

r10l10g10b10r10l10g10b10r10l10g10b10b2
""", player2).playMoves(EastSide, LeaveTaken) must beFailure
      }

    }


    "allow show sign moves" in {
      def continuePlay(side: Side, actions: Action*)(game: Valid[Game]): Valid[Game] = game flatMap { _.playMoves(side, actions:_*) }

      val firstDiscardGame = makeDuzOkeyGame("""
r13
r1r2r3
b1r13

r13
""", player).playMoves(EastSide, DrawMiddle, Discard(B1))


      val g2 = makeDuzOkeyGame("""
r13
r1r2r3r4r5
b1b2b3r13
b1b2b3
b1b2b3
b1b2b3r13
""", player).playMoves(EastSide, DrawMiddle, Discard(B1))

      val playTillSouth =
        continuePlay(NorthSide, DrawMiddle, Discard(B1))_ andThen
          continuePlay(WestSide, DrawMiddle, Discard(B1)) _

      "allow before draw middle" in {
        makeDuzOkeyGame("""
r13
r1r2r3
b1r13
""", player).playMoves(EastSide, ShowSign(R13)) must beSuccess
      }

      "allow after draw middle" in {
        makeDuzOkeyGame("""
r13
r1r2r3
b1r13
""", player).playMoves(EastSide, DrawMiddle, ShowSign(R13)) must beSuccess
      }

      "allow on first turn" in {
        continuePlay(NorthSide, ShowSign(R13))(firstDiscardGame) must beSuccess

        (playTillSouth andThen continuePlay(SouthSide, ShowSign(R13)))(g2) must beSuccess
      }

      "not allow on second turn" in {
        (playTillSouth andThen
          continuePlay(SouthSide, DrawMiddle, Discard(R13)) andThen
          continuePlay(EastSide, ShowSign(R13)))(g2) must beFailure
      }

      "not allow if not on board" in {
        makeDuzOkeyGame("""
r13
r1r2r3
b1
""", player).playMoves(EastSide, ShowSign(R13)) must beFailure
      }

      "not allow if not sign" in {
        makeDuzOkeyGame("""
r13
r1r2r3
b1r13
""", player).playMoves(EastSide, ShowSign(B1)) must beFailure
      }

    }

    "discard end" in {

      "allow discard end series" in {
        val gameEndSeries = makeDuzOkeyGame("""
r13
r1r2
r1r2r3r4r5r6g2r2b2l2l10l11l12l13b1
""", player2).playMoves(EastSide, DiscardEndSeries(L10.|>(4), R1.|>(6), Piece.<>(2)))

        gameEndSeries must beDuzOkey("""
r13
r1r2
r1r2r3r4r5r6g2r2b2l2l10l11l12l13



b1
""")

        gameEndSeries must beSuccess.like {
          case g =>
            g.situation.duzNormalEnd must_== true
            g.situation.end must_== true

        }
      }

      "allow discard end pairs" in {
        val gameEndSeries = makeDuzOkeyGame("""
r13
r1r2
r1r1r2
""", player2).playMoves(EastSide, DiscardEndPairs(R1.w))

        gameEndSeries must beDuzOkey("""
r13
r1r2
r1r1



r2
""")

        gameEndSeries must beSuccess.like {
          case g =>
            g.situation.duzPairEnd must_== true
            g.situation.end must_== true

        }
      }

      "not allow bad discard end group" in {
        makeDuzOkeyGame("""
r13
r1r2
r1r2r3r4r5r6g2r2b2l2l10l11l12l12b1
""", player2).playMoves(EastSide, DiscardEndSeries(List(L10, L11, L12, L12), R1.|>(6), Piece.<>(2))) must beFailure

      }

      "not allow bad discard end group" in {
        makeDuzOkeyGame("""
r13
r1r2
r1r3r2
""", player2).playMoves(EastSide, DiscardEndPairs(List(R2, R3))) must beFailure
      }

      "not allow if groups not enough" in {
        makeDuzOkeyGame("""
r13
r1r2
r1r2r3r4r5r6g2r2b2l2l10l11l12l13b1
""", player2).playMoves(EastSide, DiscardEndSeries(R1.|>(6), Piece.<>(2))) must beFailure
      }

      "allow 12 13 1 bind group"  in {
        makeDuzOkeyGame("""
r13
r1r2
l10l11l12l13l1b1
""", player2).playMoves(EastSide, DiscardEndSeries(List(L10, L11, L12, L13, L1))) must beSuccess
      }

      "scoring" in {
        import ScoringSystem._

        val system = okey.variant.DuzOkeyScoringSystem

        "series end" in {

          val gameEndSeries = makeDuzOkeyGame("""
r13
r1r2
r1r2r3r4r5r6g2r2b2l2l10l11l12l13b1
""", player2).playMoves(EastSide, DiscardEndSeries(L10.|>(4), R1.|>(6), Piece.<>(2)))


          gameEndSeries must beSuccess.like { case g =>
            g.situation.winner must beSome(EastSide)
            system.flags(g.situation, EastSide) must_== List(EndByHand)
            system.flags(g.situation, NorthSide) must_== List()
          }
        }

        "pairs end" in {

        val gameEndSeries = makeDuzOkeyGame("""
r13
r1r2
r1r1r2
""", player2).playMoves(EastSide, DiscardEndPairs(R1.w))

          gameEndSeries must beSuccess.like { case g =>
            g.situation.winner must beSome(EastSide)
            system.flags(g.situation, EastSide) must_== List(EndByHand, EndByPair)
            system.flags(g.situation, WestSide) must_== List()
          }
        }

        "discard okey end" in {
          val gameEndSeries = makeDuzOkeyGame("""
r13
r1r2
r1r1r1
""", player2).playMoves(EastSide, DiscardEndPairs(R1.w))

          gameEndSeries must beSuccess.like { case g =>
            system.flags(g.situation, EastSide) must_== List(EndByHand, EndByPair, EndByDiscardOkey)
            system.flags(g.situation, SouthSide) must_== List()
          }
        }

        "evaluate score sheet" should {
          import okey.variant.{ DuzOkeyScoringSystem }
          import DuzOkeyScoringSystem._

          def makeSheet(handSum: Int, flags: Flag*): EndScoreSheet =
            EndScoreSheet(handSum, flags map (f => f -> system.scorer(f, flags toList)) toMap)


          "normal end" in {
            makeSheet(12, EndByHand).total must_== -2
          }
          "pair end" in {
            makeSheet(12, EndByHand, EndByPair).total must_== -4
          }

          "discard okey" in {
            makeSheet(12, EndByHand, EndByDiscardOkey).total must_== -4
          }

          "pair discard okey" in {
            makeSheet(12, EndByHand, EndByPair, EndByDiscardOkey).total must_== 8
          }
        }
      }
    }
  }
}
