package okey

import Piece._

class OpenerTest extends OkeyTest {

  val opener = makeOpener
  val series = List(R1.|>(3), G12.<|(4))
  val series2 = List(Piece.<>(4), Piece.<>(5), R1.|>(5))
  val series3 = List(Piece.<>(13), R3.<|(5))

  val pairs = List(R1.w, G12.w)
  val pairs2 = List(R3.w, G2.w)
  val pairs3 = List(R5.w, G4.w)

  "an opener" should {
    "be empty by default" in {
      opener.series must beEmpty
      opener.pairs must beEmpty
    }

    "allow pieces to be opened series" in {
      opener.openSeries(EastSide, series) must beSuccess.like {
        case o =>
          o.series map(_.pieces) must_== series
      }
    }

    "allow pieces to be opened pairs" in {
      opener.openPairs(EastSide, pairs) must beSuccess.like {
        case o =>
          o.pairs map(_.pieces) must_== pairs
      }
    }

    "allow pieces to be opened consecutively" in {

      opener.seqOpener(
        _.openSeries(EastSide, series),
        _.openSeries(EastSide, series2),
        _.openSeries(EastSide, series3)
      ) must beSuccess.like {
        case o =>
          o.series map(_.pieces) must_== series ::: series2 ::: series3
      }

      opener.seqOpener(
        _.openPairs(EastSide, pairs),
        _.openPairs(EastSide, pairs2),
        _.openPairs(EastSide, pairs3)
      ) must beSuccess.like {
        case o =>
          o.pairs map(_.pieces) must_== pairs ::: pairs2 ::: pairs3
      }
    }
  }
}
