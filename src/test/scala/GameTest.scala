package okey

class GameTest extends OkeyTest {
  "playing a game" should {
    "draw discard" in {
      val game = makeGame

      "current game" in {
        3 must_== 3
      }
    }
  }
}
