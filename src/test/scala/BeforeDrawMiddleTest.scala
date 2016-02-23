package okey

class BeforeDrawMiddleTest extends OkeyTest {
  "before draw middle" should {
    val player = Player(side = EastSide, drawMiddle = false)
    "allow draw middle" in {
      ("""
r13
r2
""" as player) must bePoss(DrawMiddle)
    }

    "allow draw left" in {
      ("""
r13
r2







r10
""" as player) must bePoss(DrawMiddle, DrawLeft)
    }

    "not allow if middle empty" in {
      ("""
r13
""" as player) must bePoss()
    }
  }
}
