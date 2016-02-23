package okey

import Piece._

class SituationTest extends OkeyTest {

  "a game" should {

    "detect end" should {
      "by middle" in {
        val player = Player(EastSide)
          ("""
r13
""" as player).middleEnd must beTrue

        ("""
r13
r1
""" as player).middleEnd must beFalse
      }

      "by discard left" in {

        "when player board has no pieces" in {
          val player = Player(side = EastSide,
            drawMiddle = false,
            discardPiece = Discard(R1).some
          )
            ("""
r13
""" as player).discardEnd must beTrue
        }

        "when player board has pieces" in {
          val player = Player(side = EastSide,
            drawMiddle = false,
            discardPiece = Discard(R1).some
          )
            ("""
r13

r1
""" as player).discardEnd must beFalse
        }
      }
    }

    "detect turn end" should {
      "when player discards piece" in {
        val player = Player(side = EastSide,
          discardPiece = Discard(R1).some
        )
          ("""
r13
""" as player).turnEnd must beTrue
      }

      "when player hasn't discarded piece" in {
        val player = Player(side = EastSide,
          discardPiece = None
        )
          ("""
r13
""" as player).turnEnd must beFalse
      }
    }
  }
}
