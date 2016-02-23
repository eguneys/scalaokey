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

    "not allow if board is empty" in {
      ("""
r13
r2
""" as player) must bePoss()
    }
  }
}
