package okey

class AfterDrawMiddleTest extends OkeyTest {
  "after drawn middle" in {
    val player = Player(side = EastSide, drawMiddle = true)

    "allow discard and open" in {
      ("""
r13
r2
r1
""" as player) must bePoss(Discard, OpenSeries, OpenPairs)
    }

    "allow discard on old open" in {
      ("""
r13
r2
r1







er1r2r3
""" as player) must bePoss(Discard, OpenSeries)
    }

    "not allow if board is empty" in {
      ("""
r13
r2
""" as player) must bePoss()
    }
  }
}
