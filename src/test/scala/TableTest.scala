package okey

import Piece._

class TableTest extends OkeyTest {
  "a table" should {

    val table = makeTable

    val middleSize = 106 - (21 * 4 + 2)

    "position pieces correctly" in {
      "each player gets 21 piece" in {
        table.boards(EastSide).size must_== 22
        table.boards(WestSide).size must_== 21
        table.boards(NorthSide).size must_== 21
        table.boards(SouthSide).size must_== 21
      }

      "turn player gets an extra piece" in {
        val northTable = makeTable(NorthSide)

        northTable.boards(NorthSide).size must_== 22
        northTable.boards(EastSide).size must_== 21
        northTable.boards(WestSide).size must_== 21
        northTable.boards(SouthSide).size must_== 21
      }

      "have discards be empty" in {
        table.discards(EastSide) must beEmpty
        table.discards(WestSide) must beEmpty
        table.discards(NorthSide) must beEmpty
        table.discards(SouthSide) must beEmpty
      }

      "have opens be empty" in {
        table.opener must beSome.like {
          case opener =>
            opener.series must beEmpty
            opener.pairs must beEmpty
        }
      }

      "have a sign piece and an okey piece" in {
        (table.sign up) must_== table.okey
      }

      "rest of the pieces are in middle" in {
        // 106 - 21 * 4 + 2
        table.middles.size must_== middleSize

        table must haveAllPieces
      }
    }

    "allow moves" in {

      "allow a piece to be drawn middle" in {
        """
r13
r1r2r3
g1
""".drawMiddle(EastSide) must beSuccess.like {
          case t =>
            t.middles must containPieces(R2, R3)
            t.boards(EastSide) must havePieces(G1, R1)
        }
      }

      "allow a piece to be drawn left" in {
        """
r13








r1l1l2
""".drawLeft(EastSide) must beSuccess.like {
          case t =>
            t.boards(EastSide) must havePieces(R1)
            t.discards(SouthSide) must containPieces(L1, L2)
        }
      }

      "allow a piece to be discarded" in {
        """
r13

g1r1r2



l1l2
""".discard(EastSide, R1) must beSuccess.like {
          case t =>
            t.boards(EastSide) must havePieces(G1, R2)
            t.discards(EastSide) must containPieces(R1, L1, L2)
        }
      }

      "allow pieces to be opened series" in {
        """
r13

r1r2r3l1b9l2l3g1g2g3b1b2b3







eg4g5g6
""".openSeries(EastSide, List(Piece.<>(1), Piece.<>(2), Piece.<>(3))) must beSuccess.like {
          case t =>
            t.boards(EastSide) must havePieces(B9)
            t must haveOpenSeries(G4.|>(3), Piece.<>(1), Piece.<>(2), Piece.<>(3))
        }
      }


      "allow pieces to be opened pairs" in {
        """
r13

r1r2r3r1r2r3b9








eg4g4 eg5g5
""".openPairs(EastSide, List(R1.w, R2.w, R3.w)) must beSuccess.like {
          case t =>
            t.boards(EastSide) must havePieces(B9)
            t must haveOpenPairs(G4.w, G5.w, R1.w, R2.w, R3.w)
        }
      }

      "allow open series to be collected" in {
        val openPieces = List(Piece.<>(1), Piece.<>(2), Piece.<>(3))

        """
r13

r1r2r3l1b9l2l3g1g2g3b1b2b3







wg4g5g6
""".seqTable(
          _ openSeries(EastSide, openPieces),
          _ collectOpen EastSide
        ) must beSuccess.like {
          case t =>
            t.boards(EastSide) must havePieces(B9 :: openPieces.flatten)
            t must haveOpenSeries(G4.|>(3))
        }
      }

      "allow open pairs to be collected" in {
        val openPieces = List(R1.w, R2.w, R3.w)

        """
r13

r1r2r3r1b9r2r3







wg4g5g6
""".seqTable(
          _ openPairs(EastSide, openPieces),
          _ collectOpen EastSide
        ) must beSuccess.like {
          case t =>
            t.boards(EastSide) must havePieces(B9 :: openPieces.flatten)
            t must haveOpenSeries(G4.|>(3))
        }
      }


      "allow drawn piece to be left" in {
        """
r13

g1g2g3
""".leaveTaken(EastSide, G1) must beSuccess.like {
          case t =>
            t.boards(EastSide) must havePieces(G2, G3)
            t.discards(SouthSide) must containPieces(G1)
        }
      }

      "allow drawn piece to be left on open" in {
        """
r13

g1g2g3r1
""".seqTable(
          _ openPairs(EastSide, List(List(G1, R1))),
          _ leaveTaken(EastSide, G2)
        ) must beSuccess.like {
          case t =>
            t.boards(EastSide) must havePieces(G3)
            t.discards(SouthSide) must containPieces(G2)
            t must haveOpenPairs(List(G1, R1))
        }
      }


      "not allow open pieces to be collected if not opened" in {
        val openPieces = List(R1.w, R2.w, R3.w)

        """
r13

r1r2r3r1b9r2r3







wg4g5g6
""".seqTable(
          _ collectOpen EastSide
        ) must beFailure
      }

      "not allow a piece to be drawn middle if empty" in {
        """
r13""".drawMiddle(EastSide) must beFailure
      }

      "not allow a piece to be drawn left if empty" in {
        table.drawLeft(EastSide) must beFailure
      }

      "not allow a piece to be discarded if not exist" in {
        """
r13

r2r3
""".discard(EastSide, R1) must beFailure
      }

      "not allow pieces to be opened series if not exist" in {
        """
r13

r2r3
""".openSeries(EastSide, List(R1.|>(3))) must beFailure
      }

      "not allow pieces to be opened pairs if not exist" in {
        """
r13

r2r3r2
""".openPairs(EastSide, List(R2.w, R3.w)) must beFailure
      }

      "not allow drawn piece to be left if not exist" in {
        """
r13

r2r3r2
""".leaveTaken(EastSide, R1) must beFailure
      }

      "drop open pair replace okey" in {
        val openPieces = List(R2.w, List(R1, B9), R3.w)
        """
r13

r1r2r3r1b9r2r3b9







wg4g5g6
""".seqTable(_.openPairs(EastSide, openPieces),
  _.dropPairs(EastSide, B9, OpenPair(B9.w, 1), ReplaceOkey(1))
        ) must beSuccess.like {
          case t =>
            t.boards(EastSide) must havePieces(R1, R1)
            t must haveOpenPairs(R2.w, B9.w, R3.w)
        }
      }

      "drop open series replace okey" in {
        val openPieces = List(R2.|>(3))
        """
r13

r2r3r4r1b9g11







wg4g5g6 sg10r1g12
""".seqTable(_.openSeries(EastSide, openPieces),
  _.dropSeries(EastSide, G11, OpenSerie(G10.|>(3), 33), ReplaceOkey(1))
        ) must beSuccess.like {
          case t =>
            t.boards(EastSide) must havePieces(R1, R1, B9)
            t must haveOpenSeries(G4.|>(3), G10.|>(3), R2.|>(3))
        }
      }

      "drop open series append" in {
        val openPieces = List(R2.|>(3))
        """
r13

r2r3r4r1b9g13







wg4g5g6 sg10r1g12
""".seqTable(_.openSeries(EastSide, openPieces),
  _.dropSeries(EastSide, G13, OpenSerie(List(G10, R1, G12, G13), 46), AppendRight(1))
        ) must beSuccess.like {
          case t =>
            t.boards(EastSide) must havePieces(R1, B9)
            t must haveOpenSeries(G4.|>(3), List(G10, R1, G12, G13), R2.|>(3))
        }
      }

      "dont drop if no piece"  in {
        val openPieces = List(R2.|>(3))
        """
r13

r2r3r4r1b9g13







wg4g5g6 sg10r1g12
""".seqTable(_.openSeries(EastSide, openPieces),
  _.dropSeries(EastSide, G9, OpenSerie(List(G9, G10, R1, G12), 42), AppendLeft(1))
        ) must beFailure
      }
    }
  }
}
