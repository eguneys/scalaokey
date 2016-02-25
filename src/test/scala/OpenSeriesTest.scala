package okey

import Piece._

class OpenSeriesTest extends OkeyTest {
  "opening series" should {
    val player = Player(side = EastSide, drawMiddle = true)

    "when noone opened" should {
      "new open" in {
        val game = situationToGame("""
r13

r4l4g4b4r4r5r6r7r13r13
""" as player)

        "allow open series" in {
          game playMoves(EastSide, OpenSeries(Piece.<>(4))) must beGame("""
r13

r4r5r6r7r13r13







er4l4g4b4
""")
        }

        "allow multiple open series" in {
          game playMoves(EastSide,
            OpenSeries(Piece.<>(4)),
            OpenSeries(R4.|>(4))) must beGame("""
r13

r13r13







er4l4g4b4 er4r5r6r7
""")
        }

        "allow collect open for single opens" in {
          game playMoves(EastSide,
            OpenSeries(Piece.<>(4)),
            CollectOpen) must beGame("""
r13

r4l4g4b4r4r5r6r7r13r13
""")
        }

        "allow collect open for more opens" in {
          game playMoves(EastSide,
            OpenSeries(Piece.<>(4)),
            OpenSeries(R4.|>(4)),
            CollectOpen) must beGame("""
r13

r4l4g4b4r4r5r6r7r13r13
""")
        }

        "allow open series after collect open" in {
          game playMoves(EastSide,
            OpenSeries(Piece.<>(4), R4.|>(4)),
            CollectOpen,
            OpenSeries(Piece.<>(4))) must beGame("""
r13

r4r5r6r7r13r13







er4l4g4b4
""")
        }

        "allow open pairs after collect open" in {
          game playMoves(EastSide,
            OpenSeries(Piece.<>(4), R4.|>(4)),
            CollectOpen,
            OpenPairs(R13.w)) must beGame("""
r13

r4r5r6r7r4l4g4b4








er13r13
""")
        }

        "allow discard when open score above 101" in {
          val game = situationToGame("""
r13

r10l10g10b10r10l10g10b10r2r3r4r5r6r1l13
""" as player)

          game playMoves(EastSide,
            OpenSeries(Piece.<>(10), Piece.<>(10), R6.<|(6)),
            Discard(L13)) must beGame("""
r13





l13



er10l10g10b10 er10l10g10b10 er6r5r4r3r2r1
""")
        }

        "not allow open pairs after open series" in {
          game playMoves(EastSide,
            OpenSeries(Piece.<>(4)),
            OpenPairs(R13.w)) must beFailure
        }

        "not allow discard when open score below 101" in {
          val game = situationToGame("""
r13

r10l10g10b10r10l10g10b10r2r3r4r5r6l13
""" as player)

          game playMoves(EastSide,
            OpenSeries(Piece.<>(10), Piece.<>(10), R6.<|(5)),
            Discard(L13)) must beFailure
        }

      }

      "old open" in {
        val game = situationToGame("""
r13

r4l4g4b4r4r5r6r7r13r13







er10l10g10b10 er10l10g10b10 er6r5r4r3r2r1
""" as player)

        "allow open series" in {
          game playMoves(EastSide,
            OpenSeries(Piece.<>(4))) must beGame("""
r13

r4r5r6r7r13r13







er10l10g10b10 er10l10g10b10 er6r5r4r3r2r1 er4l4g4b4
""")
        }

        "not allow collect open" in {
          game playMoves(EastSide, CollectOpen) must beFailure
        }
        "not allow collect open even after open series" in {
          game playMoves(EastSide,
            OpenSeries(Piece.<>(4)),
            CollectOpen) must beFailure
        }

        "not allow open pairs" in {
          game playMoves(EastSide,
            OpenPairs(R13.w)) must beFailure
        }

        "allow discard" in {
          game playMoves(EastSide, Discard(R13)) must beGame("""
r13

r4l4g4b4r4r5r6r7r13



r13



er10l10g10b10 er10l10g10b10 er6r5r4r3r2r1
""")
        }

        "allow discard even after below 101" in {
          game playMoves(EastSide,
            OpenSeries(Piece.<>(4)),
              Discard(R13)) must beGame("""
r13

r4r5r6r7r13



r13



er10l10g10b10 er10l10g10b10 er6r5r4r3r2r1 er4l4g4b4
""")
        }
      }
    }
  }
}
