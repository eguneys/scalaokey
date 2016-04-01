package okey

class AfterDrawLeftTest extends OkeyTest {
  "after drawn left" in {
    val player = Player(side = EastSide, drawLeft = Some(Piece.R1))

    "allow open and leave taken" in {
      ("""
r13
r2
r1
""" as player) must bePoss(OpenSeries, OpenPairs, LeaveTaken)
    }

    "allow discard on old open" in {
      ("""
r13
r2
r1







er1r2r3
""" as player) must bePoss(Discard, OpenSeries, LeaveTaken, DropOpenSeries, DropOpenPairs)
    }
  }
}
