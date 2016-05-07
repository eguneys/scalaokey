package okey

import Piece._

class LeaveTakenTest extends OkeyTest {
  "leave taken" should {

    val player = Player(side = EastSide, drawLeft = Some(Piece.B2))

    "if not opened" in {
      val game = situationToGame("""
r13

r10l10g10b10r10l10g10b10r10l10g10b10b2
""" as player)

      game playMoves(EastSide,
        LeaveTaken) must beGame("""
r13

r10l10g10b10r10l10g10b10r10l10g10b10






b2
""")
    }

    "dont allow leave taken if opened old" in {
      """
r13

r10l10g10b10r10l10g10b10r10l10g10b10b2








er1r1
""" as player must bePoss(Discard, OpenPairs, DropOpenSeries, DropOpenPairs)
    }

    "leave taken and collect open if opened series" in {
      val game = situationToGame("""
r13

r10l10g10b10r10l10g10b10r10l10g10b10b2
""" as player)

      game playMoves(EastSide,
        OpenSeries(Piece.<>(10)),
        LeaveTaken) must beGame("""
r13

r10l10g10b10r10l10g10b10r10l10g10b10






b2
""")
    }
  }
}
