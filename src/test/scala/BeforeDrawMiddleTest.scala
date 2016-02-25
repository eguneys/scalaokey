package okey

class BeforeDrawMiddleTest extends OkeyTest {
  "before draw middle" should {
    val player = Player(side = EastSide, drawMiddle = false)
    "allow draw middle" in {
      val game = ("""
r13
r2
""" as player)

      game must bePoss(DrawMiddle)

      "play draw middle" in {
        situationToGame(game) playMoves(EastSide, DrawMiddle) must beGame("""
r13

r2
""")
      }
    }

    "allow draw left" in {
      val game = ("""
r13
r2







r10
""" as player)

      game must bePoss(DrawMiddle, DrawLeft)

      "play draw left" in {
        situationToGame(game) playMoves(EastSide, DrawLeft) must beGame("""
r13
r2
r10







""")
      }
    }

    "not allow if middle empty" in {
      ("""
r13
""" as player) must bePoss()
    }
  }
}
