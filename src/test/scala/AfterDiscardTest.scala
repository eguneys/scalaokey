package okey

import Piece._

class AfterDiscardTest extends OkeyTest {
  "after discard" in {
    val player = Player(side = EastSide, drawMiddle = true, discardPiece = Discard(R1).some)
    ("""
r13
r2
r1
""" as player) must bePoss()
  }
}
