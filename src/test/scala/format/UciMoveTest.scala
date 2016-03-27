package okey
package format

import Piece._

class UciMoveTest extends OkeyTest {

  "from strings" should {
    "discard" in {
      val move = Uci.Move.fromStrings(key = "dd", pieceS = Some("r1"))
      move must beSome.like { case m =>
        m.action must_== Discard(R1)
      }
    }

    "open series one" in {
      val move = Uci.Move.fromStrings(key = "os", pieceGroupS = Some("r1r2r3"))
      move must beSome.like { case m =>
        m.action must_== OpenSeries(R1.|>(3))
      }
    }

    "open series many" in {
      val move = Uci.Move.fromStrings(key = "os", pieceGroupS = Some("r1r2r3 g13g12g11g10 r10l10g10b10"))
      move must beSome.like { case m =>
        m.action must_== OpenSeries(R1.|>(3), G13.<|(4), Piece.<>(10))
      }
    }
  }

  "uci" should {
    "write" in {
      "no piece" in {
        Uci.Move(DrawMiddle).uci must_== "dm"
      }

      "single piece" in {
        Uci.Move.fromStrings(key = "dd", pieceS = Some("r1")) must beSome.like { case m =>
          m.uci must_== "ddPr1"
        }
      }

      "piece group" in {
        Uci.Move.fromStrings(key = "os", pieceGroupS = Some("r1r2r3 g1g2g3")) must beSome.like { case m =>
          m.uci must_== "osGr1r2r3 g1g2g3"
        }
      }
    }

    "read" in {
      "no piece" in {
        Uci("dm") must_== Some(Uci.Move(DrawMiddle))
      }

      "single piece" in {
        Uci("ddPr1") must beSome.like { case m =>
          m.action must_== Discard(R1)
        }
      }

      "piece group" in {
        Uci("osGr1r2r3 g1g2g3") must beSome.like { case m =>
          m.action must_== OpenSeries(R1.|>(3), G1.|>(3))
        }
      }
    }

    "list" in {
      val moves = List(DrawMiddle, Discard(R1), OpenSeries(R1.|>(3), Piece.<>(13)),
        OpenPairs(R1.w, G3.w))

      val ucis = moves.map(_.toUci)

      Uci.writeList(ucis) must_== "dm/ddPr1/osGr1r2r3 r13l13g13b13/opGr1r1 g3g3"

      Uci.readList(Uci.writeList(ucis)) must beSome.like {
        case l => l.map(_.action) must_== moves
      }
    }
  }
}
