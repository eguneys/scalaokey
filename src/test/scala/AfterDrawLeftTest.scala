package okey

class AfterDrawLeftTest extends OkeyTest {
  "after drawn left" in {
    val player = Player(side = EastSide, drawLeft = true)

    "allow open and leave taken" in {
      ("""
r13
r2
r1
""" as player) must bePoss(OpenSeries, OpenPairs, LeaveTaken)
    }
  }
}
