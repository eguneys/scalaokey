package okey

import Piece._

class OpenerTest extends OkeyTest {

  val opener = makeOpener
  val series = List(R1.|>(3), G12.<|(4)) // 1 2 3 12 11 10 9 = 48
  val series2 = List(Piece.<>(4), Piece.<>(5), R1.|>(5)) // 16 + 20 + 1 2 3 4 5 = 51
  val series3 = List(Piece.<>(13), R3.<|(5)) // 13 * 4, 3 2 1 = 58

  val pairs = List(R1.w, G12.w) // 2
  val pairs2 = List(R3.w, G2.w, G3.w) // 3
  val pairs3 = List(R5.w, G4.w, L13.w, B5.w) // 4

  val board = Board empty

  "an opener" should {
    "be empty by default" in {
      opener.series must beEmpty
      opener.pairs must beEmpty
    }

    "allow pieces to be opened series" in {
      opener.openSeries(EastSide, series, board) must beSome.like {
        case o =>
          o.series map(_.pieces) must_== series
      }
    }

    "allow pieces to be opened pairs" in {
      opener.openPairs(EastSide, pairs, board) must beSome.like {
        case o =>
          o.pairs map(_.pieces) must_== pairs
      }
    }

    "allow pieces to be opened consecutively" in {

      opener.seqOpener(
        _.openSeries(EastSide, series, board),
        _.openSeries(EastSide, series2, board),
        _.openSeries(EastSide, series3, board)
      ) must beSome.like {
        case o =>
          o.series map(_.pieces) must_== series ::: series2 ::: series3
      }

      opener.seqOpener(
        _.openPairs(EastSide, pairs, board),
        _.openPairs(EastSide, pairs2, board),
        _.openPairs(EastSide, pairs3, board)
      ) must beSome.like {
        case o =>
          o.pairs map(_.pieces) must_== pairs ::: pairs2 ::: pairs3
      }
    }

    "allow opener to collect opened" in {
      opener.seqOpener(
        _.openSeries(EastSide, series, board),
        _.collectOpen(EastSide)
      ) must beSome.like {
        case o =>
          o must_== opener
      }
    }

    "allow opener to collect concecutive opened" in {
      opener.seqOpener(
        _.openSeries(EastSide, series, board),
        _.openSeries(EastSide, series2, board),
        _.openSeries(EastSide, series3, board),
        _.collectOpen(EastSide)
      ) must beSome.like {
        case o =>
          o must_== opener
      }
    }

    "not allow opener to collect opened after commit" in {
      opener.seqOpener(
        _.openSeries(EastSide, series, board),
        _.commitOpen(EastSide),
        _.collectOpen(EastSide)
      ) must beNone
    }

    "save board" in {
      opener.openSeries(EastSide, series, board) must beSome.like
      { case o => o.boardSave(EastSide) must beSome }

      opener.seqOpener(
        _.openSeries(EastSide, series, board),
        _.openSeries(EastSide, series, board)
      ) must beSome.like { case o => o.boardSave(EastSide) must beSome }

      opener.seqOpener(
        _.openSeries(EastSide, series, board),
        _.openSeries(EastSide, series, board),
        _.collectOpen(EastSide)
      ) must beSome.like { case o => o.boardSave(EastSide) must beNone }

      opener.seqOpener(
        _.openSeries(EastSide, series, board),
        _.openSeries(EastSide, series, board),
        _.commitOpen(EastSide)
      ) must beSome.like { case o => o.boardSave(EastSide) must beNone }
    }

    "score calculation" should {
      "open one series" in {
        opener.openSeries(EastSide, series, board) must beSome.like {
          case o =>
            o.score(EastSide) must beSome(48)
        }
      }

      "open more series" in {
        opener.seqOpener(
          _.openSeries(EastSide, series, board),
          _.openSeries(EastSide, series2, board),
          _.openSeries(EastSide, series3, board)
        ) must beSome.like {
          case o =>
            o.score(EastSide) must beSome(48 + 51 + 58)
        }
      }

      "open one pairs" in {
        opener.openPairs(EastSide, pairs, board) must beSome.like {
          case o =>
            o.score(EastSide) must beSome(2)
        }
      }

      "open more pairs" in {
        opener.seqOpener(
          _.openPairs(EastSide, pairs, board),
          _.openPairs(EastSide, pairs2, board),
          _.openPairs(EastSide, pairs3, board)
        ) must beSome.like {
          case o =>
            o.score(EastSide) must beSome(2 + 3 + 4)
        }
      }

      "open series after commit" in {
        opener.seqOpener(
          _.openSeries(EastSide, series, board),
          _.openSeries(EastSide, series2, board),
          _.commitOpen(EastSide),
          _.openSeries(EastSide, series3, board)
        ) must beSome.like {
          case o =>
            o.score(EastSide) must beSome(48 + 51)
        }
      }

      "open pairs after commit" in {
        opener.seqOpener(
          _.openPairs(EastSide, pairs, board),
          _.openPairs(EastSide, pairs2, board),
          _.commitOpen(EastSide),
          _.openPairs(EastSide, pairs3, board)
        ) must beSome.like {
          case o =>
            o.score(EastSide) must beSome(2 + 3)
        }
      }
    }
  }
}
