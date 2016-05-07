package okey

import Piece._

class AfterDrawLeftTest extends OkeyTest {
  "after drawn left" in {
    val player = Player(side = EastSide, drawLeft = Some(R1))

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
""" as player) must bePoss(Discard, OpenSeries, DropOpenSeries, DropOpenPairs)
    }
  }

  "allow discard if open" should {
    val player = Player(side = EastSide, drawLeft = Some(Piece.B2))
    "open series" in {
      val game = situationToGame("""
r13

r10l10g10b10r10l10g10b10r10l10g10b10b2
""" as player)

      game playMoves(EastSide,
        OpenSeries(Piece.<>(10), Piece.<>(10), Piece.<>(10)),
        Discard(B2)) must beGame("""
r13





b2



er10l10g10b10 er10l10g10b10 er10l10g10b10
""")
    }

    "open pairs" in {
      val game = situationToGame("""
r13

r1r1r2r2r3r3r4r4r5r5b2
""" as player)

      game playMoves(EastSide,
        OpenPairs(R1.w, R2.w, R3.w, R4.w, R5.w),
        Discard(B2)) must beGame("""
r13





b2




er1r1 er2r2 er3r3 er4r4 er5r5
""")
    }
  }
}
