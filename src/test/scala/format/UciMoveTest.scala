package okey
package format

import Piece._

class UciMoveTest extends OkeyTest {

  "from strings" should {
    "discard" in {
      val move = Uci.Move.fromStrings(key = "dd", pieceS = Some("r1"))
      move must beSome.like {
        case Uci.Move(action) => action must_== Discard(R1)
      }
    }

    "open series one" in {
      val move = Uci.Move.fromStrings(key = "os", pieceGroupS = Some("r1r2r3"))
      move must beSome.like {
        case Uci.Move(action) => action must_== OpenSeries(R1.|>(3))
      }
    }

    "open series many" in {
      val move = Uci.Move.fromStrings(key = "os", pieceGroupS = Some("r1r2r3 g13g12g11g10 r10l10g10b10"))
      move must beSome.like {
        case Uci.Move(action) => action must_== OpenSeries(R1.|>(3), G13.<|(4), Piece.<>(10))
      }
    }
  }
}
