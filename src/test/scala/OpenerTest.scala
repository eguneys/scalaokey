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
          o.series map(_._2.pieces) must_== series
      }
    }

    "allow pieces to be opened pairs" in {
      opener.openPairs(EastSide, pairs, board) must beSome.like {
        case o =>
          o.pairs map(_._2.pieces) must_== pairs
      }
    }

    "allow pieces to be opened consecutively" in {

      opener.seqOpener(
        _.openSeries(EastSide, series, board),
        _.openSeries(EastSide, series2, board),
        _.openSeries(EastSide, series3, board)
      ) must beSome.like {
        case o =>
          o.series map(_._2.pieces) must_== series ::: series2 ::: series3
      }

      opener.seqOpener(
        _.openPairs(EastSide, pairs, board),
        _.openPairs(EastSide, pairs2, board),
        _.openPairs(EastSide, pairs3, board)
      ) must beSome.like {
        case o =>
          o.pairs map(_._2.pieces) must_== pairs ::: pairs2 ::: pairs3
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

      opener.boardSave(EastSide) must beNone
    }

    "score calculation" should {
      "open one series" in {
        opener.openSeries(EastSide, series, board) must beSome.like {
          case o =>
            o.score(EastSide) must beSome(SerieScore(48))
        }
      }

      "open more series" in {
        opener.seqOpener(
          _.openSeries(EastSide, series, board),
          _.openSeries(EastSide, series2, board),
          _.openSeries(EastSide, series3, board)
        ) must beSome.like {
          case o =>
            o.score(EastSide) must beSome(SerieScore(48 + 51 + 58))
        }
      }

      "open one pairs" in {
        opener.openPairs(EastSide, pairs, board) must beSome.like {
          case o =>
            o.score(EastSide) must beSome(PairScore(2))
        }
      }

      "open more pairs" in {
        opener.seqOpener(
          _.openPairs(EastSide, pairs, board),
          _.openPairs(EastSide, pairs2, board),
          _.openPairs(EastSide, pairs3, board)
        ) must beSome.like {
          case o =>
            o.score(EastSide) must beSome(PairScore(2 + 3 + 4))
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
            o.score(EastSide) must beSome(SerieScore(48 + 51))
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
            o.score(EastSide) must beSome(PairScore(2 + 3))
        }
      }

      "max open scores" in {
        val table = """
r13









er10l10g10b10 wr11l11g11b11 wr12l12g12b12 er13l13g13b13 er1r2r3r4
sr10r10 wl10l10 sg10g10 sb10b10 wl10l10
"""

        "serie score" in {
          table.opener must beSome.like {
            case o => o.maxOpenSerieScore must beSome(44 + 48 + 10)
          }
        }

        "pair score" in {
          table.opener must beSome.like {
            case o => o.maxOpenPairScore must beSome(3)
          }
        }
      }
    }

    "drop update series" in {
        val table = """
r13









er10l10g10b10 wr11l11g11b11 wr12l12g12b12 er13l13g13b13 er1r2r3r4
sr10r10 wl10l10 sg10g10 sb10b10 wl10l10
"""

      (table.opener get).updateSeries(OpenSerie(R1.|>(5), 15), 4) must beSome.like {
        case o =>
          o.series.lift(4) must beSome((EastSide -> OpenSerie(R1.|>(5), 15)))
          o.score(EastSide) must beSome(SerieScore(102))
      }
    }

    "drop update pairs" in {
        val table = """
r13









er10l10g10b10 wr11l11g11b11 wr12l12g12b12 er13l13g13b13 er1r2r3r4
sr1r10 wl10l10 sg10g10 sb10b10 wl10l10
"""

      (table.opener get).updatePairs(OpenPair(R10.w, 1), 0) must beSome.like {
        case o =>
          o.pairs.lift(0) must beSome((SouthSide -> OpenPair(R10.w, 1)))
          o.score(SouthSide) must beSome(PairScore(3))
      }
    }


    "recalculate new open score on drop update" in {
      opener.seqOpener(
        _.openSeries(EastSide, series, board),
        _.updateSeries(OpenSerie(R1.|>(4), 10), 0)
      ) must beSome.like {
        case o =>
          o.series.lift(0) must beSome((EastSide -> OpenSerie(R1.|>(4), 10)))
          o.score(EastSide) must beSome(SerieScore(48 + 4))
      }
    }

    // sign R13  by default
    "recalculate new open score on drop okey replace" in {
      opener.seqOpener(
        _.openSeries(EastSide, series, board),
        _.openSeries(EastSide, List(List(R10, R1, R12)), board),
        _.updateSeries(OpenSerie(R10.|>(3), 33), 2)
      ) must beSome.like {
        case o =>
          o.series.lift(2) must beSome((EastSide -> OpenSerie(R10.|>(3), 33)))
          o.score(EastSide) must beSome(SerieScore(48 + 33))
      }
    }

  }
}
