package okey

import Piece._

class DropOpenPairsTest extends OkeyTest {

  "drop open pair" should {
    val player = Player(side = EastSide, drawMiddle = true)

    "allow drop open pair on new open" in {
      val game = situationToGame("""
r13
l1l2
r1r1r2r2r3r3
""" as player)

      game playMoves(EastSide, OpenPairs(R2.w, List(R3, R1)), DropOpenPairs(R3, ReplaceOkey(1))) must beGame("""
r13
l1l2
r1r1








er2r2 er3r3
""")
    }

    "allow drop open pair on old open" in {
      val game = situationToGame("""
r13
l1l2
r4r7r13r13r8r9g2







eg10l10b10 wr1r2r3
sr1g2
""" as player)

      game playMoves(EastSide, DropOpenPairs(G2, ReplaceOkey(0))) must beGame("""
r13
l1l2
r4r7r13r13r8r9r1







eg10l10b10 wr1r2r3
sg2g2
""")
    }

    "allow drop open pairs if opened pairs" in {
      val game = situationToGame("""
r13
l1l2
r7r13r13r8r9


g10




wg9r1g11
sr1g1 er1g10
""" as Player(SouthSide, drawMiddle = true))

      game playMoves(SouthSide, DropOpenPairs(G10, ReplaceOkey(1))) must beGame("""
r13
l1l2
r7r13r13r8r9


r1




wg9r1g11
sr1g1 eg10g10
""")
    }

    "not allow drop open pairs if not opened" in {
      val game = situationToGame("""
r13
l1l2
r7r13r13r8r9


g10r2




wg9r1g11
er1r2
""" as Player(SouthSide, drawMiddle = true))

      game playMoves(SouthSide, DropOpenPairs(R2, ReplaceOkey(0))) must beFailure
    }
  }
}
