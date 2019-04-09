package okey

import Piece._

import scalaz.Validation.FlatMap._

class PlayTest extends OkeyTest {
  "playing a game" should {

    def continuePlay(game: Valid[Game], side: Side, actions: Action*): Valid[Game] = game flatMap { _.playMoves(side, actions:_*) }

    // def playMoves(game: Game, turns: (Side, List[Action])*): Valid[Game] = ???

    "change turns on discard" in {
      val game = situationToGame("""
r13
l1l2l3l4b1b2b3b4
r1r2r3r4r5r6r7r8r9
r1l1g1b1

r13
""" as Player(EastSide))

      val turn1 = game.playMoves(EastSide, DrawMiddle, Discard(L1))
      val turn2 = continuePlay(turn1, NorthSide, DrawMiddle, Discard(L2))
      val turn3 = continuePlay(turn2, WestSide, DrawLeft, LeaveTaken, DrawMiddle, Discard(L3))
      val invalidTurn4s = List(
        continuePlay(turn3, SouthSide, Discard(R13)),
        continuePlay(turn3, SouthSide, DrawLeft, Discard(R13)),
        continuePlay(turn3, SouthSide, DrawLeft, LeaveTaken, Discard(R13)),
        continuePlay(turn3, SouthSide, DrawLeft, LeaveTaken, Discard(R13)),
        continuePlay(turn3, SouthSide, DrawLeft, LeaveTaken, DrawLeft, Discard(R13)))

      "east turn 1" in {

        turn1 must beGame("""
r13
l2l3l4b1b2b3b4
r1r2r3r4r5r6r7r8r9
r1l1g1b1

r13
l1
""")
      }

      "north turn 2" in {
        turn2 must beGame("""
r13
l3l4b1b2b3b4
r1r2r3r4r5r6r7r8r9
r1l1g1b1

r13
l1

l2
""")
      }

      "draw left and leave taken" in {
        "west turn 3 allow draw left, leave taken, draw middle" in {
          turn3 must beGame("""
r13
l4b1b2b3b4
r1r2r3r4r5r6r7r8r9
r1l1g1b1

r13
l1
l3
l2
""")
        }

        "south turn 4 not allow draw left, early discard" in {
          invalidTurn4s map { _ must beFailure }
        }
      }
    }

//     "calculate scores" in {
//       val game = situationToGame("""
// r13
// l1l1l1l1l1l1l1l1l1l1l1l1
// r10l10g10b10r10l10g10b10r10l10g10b10
// r10l10g10b10r10l10g10b10r10l10g10b10
// r10l10g10b10r10l10g10b10r10l10g10b10
// r10l10g10b10r10l10g10b10r10l10g10b10
// """ as Player(EastSide))

//       val eastOpenSeries120 = game.playMoves(EastSide, DrawMiddle, OpenSeries(Piece.<>(10), Piece.<>(10), Piece.<>(10)), Discard(L1))

//       "game1" in {
//         val turn2 = continuePlay(turn1, NorthSide, DrawMiddle, Discard(L1))

//       }
//     }

  }
}
