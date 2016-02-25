package okey

import Piece._

class OpenPairsTest extends OkeyTest {
  "opening pairs" should {
    val player = Player(side = EastSide, drawMiddle = true)

    "when noone opened" in {

      "new open" in {
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

        "allow discard when open score above 5" in {
          val game = situationToGame("""
r13

r1r2r3r4r5r1r2r3r4r5r13r13
""" as player)

          game playMoves(EastSide,
            OpenPairs(R1.w, R2.w, R3.w, R4.w, R5.w),
            Discard(R13)) must beGame("""
r13

r13



r13




er1r1 er2r2 er3r3 er4r4 er5r5
""")
        }

        "not allow discard when open score below 5" in {
          val game = situationToGame("""
r13

r1r2r3r4r5r1r2r3r4r5r13r13
""" as player)

          game playMoves(EastSide,
            OpenPairs(R1.w, R2.w, R3.w, R4.w),
            Discard(R13)) must beFailure
        }

        "not allow open series after open pairs" in {
          game playMoves(EastSide,
            OpenPairs(G8.w),
            OpenSeries(R4.|>(4))) must beFailure
        }
      }

      "old open" in {
        val game = situationToGame("""
r13

r1r2r3r4r5r6r1r2r3r4r5r6r13r13








er1r1 er2r2 er3r3 er4r4 er5r5
""" as player)

        "allow open pairs" in {
          game playMoves(EastSide,
            OpenPairs(R1.w)) must beGame("""
r13

r2r3r4r5r6r2r3r4r5r6r13r13








er1r1 er2r2 er3r3 er4r4 er5r5 er1r1
""")
        }

        "not allow collect open" in {
          game playMoves(EastSide, CollectOpen) must beFailure
        }

        "not allow collect open even after open pairs" in {
          game playMoves(EastSide,
            OpenPairs(R1.w),
            CollectOpen) must beFailure
        }

        "not allow open series" in {
          game playMoves(EastSide,
            OpenSeries(R2.|>(3))) must beFailure
        }
        "allow discard" in {
          game playMoves(EastSide, Discard(R13)) must beGame("""
r13

r1r2r3r4r5r6r1r2r3r4r5r6r13



r13




er1r1 er2r2 er3r3 er4r4 er5r5
""")
        }

        "allow discard even after below 5" in {
          game playMoves(EastSide,
            OpenPairs(R1.w),
            Discard(R13)) must beGame("""
r13

r2r3r4r5r6r2r3r4r5r6r13



r13




er1r1 er2r2 er3r3 er4r4 er5r5 er1r1
""")
        }
      }
    }

    "someone opened pairs" in {
      val game = situationToGame("""
r13

r1r2r3r4r5r6r7r1r2r3r4r5r6r7l13l10








wr1r1 wr2r2 wr3r3 wr4r4 wr5r5 wr6r6
""" as player) // West Score = 6

      "new open" in {
        "allow discard when open score above 6" in {
          game playMoves(EastSide,
            OpenPairs(R1.w, R2.w, R3.w, R4.w, R5.w, R6.w, R7.w),
            Discard(L13)) must beGame("""
r13

l10



l13




wr1r1 wr2r2 wr3r3 wr4r4 wr5r5 wr6r6 er1r1 er2r2 er3r3 er4r4 er5r5 er6r6 er7r7
""")
        }

        "not allow discard when open score below 6" in {
          game playMoves(EastSide,
            OpenPairs(R1.w, R2.w, R3.w, R4.w, R5.w, R6.w),
            Discard(L13)) must beFailure
        }
      }
    }
  }
}
