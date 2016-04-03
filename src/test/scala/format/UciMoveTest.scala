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

      "drop open series" in {
        Uci.Move.fromStrings(key = "dos", pieceS = Some("r1"), posS = Some("l1")) must beSome.like { case m =>
          m.uci must_== "dosPr1@l1"
        }
        Uci.Move.fromStrings(key = "dos", pieceS = Some("r1"), posS = Some("r9")) must beSome.like { case m =>
          m.uci must_== "dosPr1@r9"
        }
        Uci.Move.fromStrings(key = "dos", pieceS = Some("r1"), posS = Some("p10")) must beSome.like { case m =>
          m.uci must_== "dosPr1@p10"
        }
      }

      "drop open pairs" in {
        Uci.Move.fromStrings(key = "dop", pieceS = Some("r1"), posS = Some("r9")) must beSome.like { case m =>
          m.uci must_== "dopPr1@r9"
        }
      }

      "dos dop" in {
        val dos = DropOpenSeries(R1, AppendLeft(2))
        dos.toUci must_== Uci.Move(dos, piece = Some(R1), openPos = Some(AppendLeft(2)))
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

      "open pos" in {
        Uci("dopPr1@p9") must beSome.like { case m =>
          m.action must_== DropOpenPairs(R1, ReplaceOkey(9))
        }
        Uci("dosPr1@l10") must beSome.like { case m =>
          m.action must_== DropOpenSeries(R1, AppendLeft(10))
        }
        Uci("dosPr1@r11") must beSome.like { case m =>
          m.action must_== DropOpenSeries(R1, AppendRight(11))
        }
      }
    }

    "list" in {
      val moves = List(DrawMiddle, Discard(R1), OpenSeries(R1.|>(3), Piece.<>(13)),
        OpenPairs(R1.w, G3.w), DropOpenSeries(R1, AppendLeft(2)), DropOpenPairs(F1, ReplaceOkey(10)))

      val ucis = moves.map(_.toUci)

      Uci.writeList(ucis) must_== "dm/ddPr1/osGr1r2r3 r13l13g13b13/opGr1r1 g3g3/dosPr1@l2/dopPf1@p10"

      Uci.readList(Uci.writeList(ucis)) must beSome.like {
        case l => l.map(_.action) must_== moves
      }

      "with fake" in {
        val moves = List(Discard(F1), OpenSeries(List(F1, G1, R13)), OpenPairs(F1.w, G1.w), DropOpenSeries(R1, AppendRight(9)), DropOpenPairs(R13, ReplaceOkey(10)))

        val ucis = moves.map(_.toUci)

        Uci.writeList(ucis) must_== "ddPf1/osGf1g1r13/opGf1f1 g1g1/dosPr1@r9/dopPr13@p10"

        Uci.readList(Uci.writeList(ucis)) must beSome.like {
          case l => l.map(_.action) must_== moves
        }
      }
    }
  }
}
