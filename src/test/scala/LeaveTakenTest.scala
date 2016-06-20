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

    "leave taken if opened series" in {
      val game = situationToGame("""
r13

r10l10g10b10r10l10g10b10r10l10g10b10b2
""" as player)

      game playMoves(EastSide,
        OpenSeries(Piece.<>(10)),
        LeaveTaken) must beGame("""
r13

r10l10g10b10r10l10g10b10






b2
er10l10g10b10
""")
    }

    "after leave taken" should {
      "if not opened" in {
        val leftTakenGame = situationToGame("""
r13
g1g2g3
r10l10g10b10r10l10g10b10r10l10g10b10b2
""" as player) playMoves(EastSide, LeaveTaken)

        leftTakenGame must beSuccess.like { case g =>
          g.situation must bePoss(DrawMiddle, DrawLeft)
        }
      }

      "if opened new" in {
        def continuePlay(game: Valid[Game], side: Side, actions: Action*): Valid[Game] = game flatMap { _.playMoves(side, actions:_*) }

        val leftTakenGame = situationToGame("""
r13
g1g2g3
r10l10g10b10r10l10g10b10r10l10g10b10b2b3
""" as player) playMoves(EastSide,
  OpenSeries(Piece.<>(10)),
  OpenSeries(Piece.<>(10)),
  OpenSeries(Piece.<>(10)), LeaveTaken)

        leftTakenGame must beGame("""
r13
g1g2g3
b3






b2
er10l10g10b10 er10l10g10b10 er10l10g10b10
""")

        leftTakenGame must beSuccess.like { case g =>
          g.situation must bePoss(DrawMiddle, DrawLeft)
        }

        "allow collect open after second draw" in {
          continuePlay(leftTakenGame, EastSide, DrawMiddle) must beSuccess.like { case g =>
            g.situation must bePoss(CollectOpen, Discard, OpenSeries, DropOpenSeries, DropOpenPairs)
          }
        }
      }
    }
  }
}
