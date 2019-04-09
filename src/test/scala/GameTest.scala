package okey

import Piece._

import scalaz.Validation.FlatMap._

class GameTest extends OkeyTest {
  "playing a game" should {

    def continuePlay(game: Valid[Game], side: Side, actions: Action*): Valid[Game] = game flatMap { _.playMoves(side, actions:_*) }

    val emptyOpens = Sides[Option[Opens]]

    def makeOpens(from: Sides[Option[Opens]], side: Side, pairs: Boolean, old: Boolean) = from.withSide(side, Opens(pairs, old).some)

    "record last moves" in {
      val game = situationToGame("""
r13
l1l2l3l4b1b2b3b4
r10g10b10l10r10g10b10l10r10g10b10l10l1
r1l1g1b1
g1
r13
""" as Player(EastSide))

      val g2 = game.playMoves(EastSide, DrawMiddle, Discard(R10))


      "save until discard" in {
        g2 must haveLastMoves(DrawMiddle(L1), Discard(R10))
      }

      "reset after draw middle" in {
        continuePlay(g2, NorthSide, DrawMiddle) must haveLastMoves(DrawMiddle(L2))
      }

      "reset after draw left" in {
        continuePlay(g2, NorthSide, DrawLeft) must haveLastMoves(DrawLeft(R10))
      }

      "save opens" in {
        val moves = List(OpenSeries(Piece.<>(10), Piece.<>(10), Piece.<>(10)), Discard(L1))
        game.playMoveList(EastSide, DrawMiddle :: moves) must haveLastMoves((DrawMiddle(L1) :: moves):_*)
      }
    }

    "save open states" in {
      val game = situationToGame("""
r13
l1l2l3l4b1b2b3b4
r10g10b10l10r10g10b10l10r10g10b10l10l1
r1l1g1b1
g1g1g2g2g3g3g4g4g5g5g6
r13
""" as Player(EastSide))

      val eastOpen = OpenSeries(Piece.<>(10), Piece.<>(10), Piece.<>(10))
      val northOpen = OpenPairs(G1.w, G2.w, G3.w, G4.w, G5.w)

      "empty by default" in {
        game.player.history.openStates must_== emptyOpens
      }

      "before discard" in {
        game.playMoves(EastSide, DrawMiddle, eastOpen) must haveOpenStates(emptyOpens)
      }
      "after discard" in {
        val afterDiscardTurn1 = game.playMoves(EastSide, DrawMiddle, eastOpen, Discard(L1))

        "it saves new open" in {
          afterDiscardTurn1 must haveOpenStates(makeOpens(emptyOpens, EastSide, old = false, pairs = false))
        }

        "after discard draw middle" in {
          continuePlay(afterDiscardTurn1, NorthSide, DrawMiddle) must haveOpenStates(makeOpens(emptyOpens, EastSide, old = false, pairs = false))
        }

        "after discard second open" in {
          continuePlay(afterDiscardTurn1, NorthSide, DrawMiddle, northOpen) must haveOpenStates(makeOpens(emptyOpens, EastSide, old = false, pairs = false))
        }

        "after second discard" in {
          val eastOldOpens = makeOpens(emptyOpens, EastSide, old = true, pairs = false)
          continuePlay(afterDiscardTurn1, NorthSide, DrawMiddle, northOpen, Discard(G6)) must haveOpenStates(makeOpens(eastOldOpens, NorthSide, old = false, pairs = true))
        }
      }
    }

    "hand scores" in {
      "normal end" in {
        val game = situationToGame("""
r13
l1
l7l9r3r4
l7l9r3r1
l7l9r3r4





er1r2r3 wr1r2r3 sr1r2r3
nr1r1
""" as Player(SouthSide))

        val endGame = game.playMoves(SouthSide, DrawMiddle, Discard(L1))

        "open player gets hand sum score" in {
          // hand okey player gets + 101
          // pair open player gets double
          // hand zero player gets -101
          endGame must haveScores(Sides(23, 19 + 101, 23 * 2, -101))
        }
      }

      "okey end" in {
        val game = situationToGame("""
r13
r1
l7l9r3r4
l7l9r3r1
l7l9r3r4





er1r2r3 wr1r2r3 sr1r2r3
nr1r1
""" as Player(SouthSide))

        val endGame = game.playMoves(SouthSide, DrawMiddle, Discard(R1))

        //"open player gets hand sum score"
        // hand okey player gets + 101
        // pair open player gets double
        // hand zero player gets -101
        endGame must haveScores(Sides(23, 19 + 101, 23 * 2, -101) map(_*2))
      }

      "hand end" in {
        val game = situationToGame("""
r13
l1
l7l9r3r4
l7l9r3r1
l7l9r3r4
l10g10r10b10l10g10r10b10l10g10r10b10
""" as Player(SouthSide))

        val endGame = game.playMoves(SouthSide,
          DrawMiddle,
          OpenSeries(Piece.<>(10), Piece.<>(10), Piece.<>(10)),
          Discard(L1))

        endGame must haveScores(Sides(101, 101, 101, -101) map (_*2))
      }

      "hand end okey discard" in {
        val game = situationToGame("""
r13
r1
l7l9r3r4
l7l9r3r1
l7l9r3r4
l10g10r10b10l10g10r10b10l10g10r10b10
""" as Player(SouthSide))

        val endGame = game.playMoves(SouthSide,
          DrawMiddle,
          OpenSeries(Piece.<>(10), Piece.<>(10), Piece.<>(10)),
          Discard(R1))

        endGame must haveScores(Sides(101, 101, 101, -101) map (_*4))
      }

      "pair end" in {
        val game = situationToGame("""
r13
l1l2l3
l7l7r3r3r2r2r1r1r4r4
l7l9r3r1
l7l9r3r4
l10g10r10b10l10g10r10b10l10g10r10b10b13





wr1r1 wr2r2
""" as Player(SouthSide))

        val g2 = game.playMoves(SouthSide,
          DrawMiddle,
          OpenSeries(Piece.<>(10), Piece.<>(10), Piece.<>(10)),
          Discard(L1))

        val endGame = continuePlay(g2, EastSide,
          DrawMiddle,
          OpenPairs(L7.w, R1.w, R2.w, R3.w, R4.w),
          Discard(L2))

        // hand zero / hand open pair / end by pair = gets -101  not -404
        // west => hand okey / hand open pair
        endGame must haveScores(Sides(-101, (19 + 101) * 4, 101 * 2, 13 * 2))
      }.pendingUntilFixed
    }
  }
}
