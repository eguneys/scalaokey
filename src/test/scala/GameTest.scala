package okey

import Piece._

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
  }
}
