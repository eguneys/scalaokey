package okey

import Piece._

class OpenPairsTest extends OkeyTest {
  "opening pairs" should {
    val player = Player(side = EastSide, drawMiddle = true)

    "when noone opened" in {
      val game = situationToGame("""
r13

r4r5r6r7g7g7g8g8g9g9
""" as player)

      "allow open pairs" in {
        game playMoves(EastSide, OpenPairs(G8.w)) must beGame("""
r13

r4r5r6r7g7g7g9g9








eg8g8
""")
      }

      "allow multiple open pairs" in {
        game playMoves(EastSide,
          OpenPairs(G8.w),
          OpenPairs(G9.w)) must beGame("""
r13

r4r5r6r7g7g7








eg8g8 eg9g9
""")
      }

      "allow collect open for single opens" in {
        game playMoves(EastSide,
          OpenPairs(G8.w),
          CollectOpen) must beGame("""
r13

r4r5r6r7g7g7g8g8g9g9
""")
      }

      "allow collect open for more opens" in {
        game playMoves(EastSide,
          OpenPairs(G8.w),
          OpenPairs(G9.w),
          CollectOpen) must beGame("""
r13

r4r5r6r7g7g7g8g8g9g9
""")
      }

      "allow open pairs after collect open" in {
        game playMoves(EastSide,
          OpenPairs(G8.w, G9.w),
          CollectOpen,
          OpenPairs(G8.w)) must beGame("""
r13

r4r5r6r7g7g7g9g9








eg8g8
""")
      }

      "allow open series after collect open" in {
        game playMoves(EastSide,
          OpenPairs(G8.w, G9.w),
          CollectOpen,
          OpenSeries(R4.|>(4))) must beGame("""
r13

g7g7g8g8g9g9







er4r5r6r7
""")
      }

      "not allow open series after open pairs" in {
        game playMoves(EastSide,
          OpenPairs(G8.w),
          OpenSeries(R4.|>(4))) must beFailure
      }
    }
  }
}
