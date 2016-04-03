package okey

import Piece._

class DropOpensTest extends OkeyTest {

  "drop open serie" should {
    val player = Player(side = EastSide, drawMiddle = true)

    "allow drop open series on new open" in {
      val game = situationToGame("""
r13
l1l2
r3r4r5r6r7r13r13r8r9
""" as player)

      game playMoves(EastSide, OpenSeries(R4.|>(3)), DropOpenSeries(R3, AppendLeft(0))) must beGame("""
r13
l1l2
r7r13r13r8r9







er3r4r5r6
""")
    }

    "allow drop open series on old open" in {
      val game = situationToGame("""
r13
l1l2
r4r7r13r13r8r9







eg10l10b10 wr1r2r3
""" as player)

      game playMoves(EastSide, DropOpenSeries(R4, AppendRight(1))) must beGame("""
r13
l1l2
r7r13r13r8r9







eg10l10b10 wr1r2r3r4
""")
    }

    "allow drop open replace okey" in {
      val game = situationToGame("""
r13
l1l2
r7r13r13r8r9g10







er4r5r6 wg9r1g11
""" as player)

      game playMoves(EastSide, DropOpenSeries(G10, ReplaceOkey(1))) must beGame("""
r13
l1l2
r7r13r13r8r9r1







er4r5r6 wg9g10g11
""")
    }

    "allow drop open fake" in {
      val game = situationToGame("""
r13
l1l2
r7r13r13r8r9f1







er4r5r6 wr2r3r4
""" as player)

      game playMoves(EastSide, DropOpenSeries(F1, AppendLeft(1))) must beGame("""
r13
l1l2
r7r13r13r8r9







er4r5r6 wf1r2r3r4
""")
    }


    "allow drop open series if opened pairs" in {
      val game = situationToGame("""
r13
l1l2
r7r13r13r8r9


g10




er4r5r6 wg9r1g11
sr1r1
""" as Player(SouthSide, drawMiddle = true))

      game playMoves(SouthSide, DropOpenSeries(G10, ReplaceOkey(1))) must beGame("""
r13
l1l2
r7r13r13r8r9


r1




er4r5r6 wg9g10g11
sr1r1
""")
    }

    "not allow drop open series if not opened" in {
      val game = situationToGame("""
r13
l1l2
r7r13r13r8r9


g10




er4r5r6 wg9r1g11
""" as Player(SouthSide, drawMiddle = true))

      game playMoves(SouthSide, DropOpenSeries(G10, ReplaceOkey(1))) must beFailure
    }

    "not allow drop open series if no piece" in {
      val game = situationToGame("""
r13
l1l2
r7r13r13r8r9


l10




er4r5r6 wg9r1g11 sr1r2r3
""" as Player(SouthSide, drawMiddle = true))

      game playMoves(SouthSide, DropOpenSeries(G10, ReplaceOkey(1))) must beFailure
    }

    "not allow drop open series if bad serie" in {
      val game = situationToGame("""
r13
l1l2
r7r13r13r8r9


g13




er4r5r6 wg9r1g11 sr1r2r3
""" as Player(SouthSide, drawMiddle = true))

      game playMoves(SouthSide, DropOpenSeries(G13, AppendRight(1))) must beFailure
    }
  }
}
