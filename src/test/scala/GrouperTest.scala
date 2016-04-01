package okey

import Piece._

class GrouperTest extends OkeyTest {
  val grouper = StandardGrouper(R2)

  val appendRight = AppendRight(1)
  val appendLeft = AppendLeft(1)

  "grouper" should {
    "groups number series" in {
      grouper.series(L1.|>(13)) must beSome(OpenSerie(L1.|>(13), 91))
      grouper.series(L13.<|(13)) must beSome(OpenSerie(L13.<|(13), 91))
    }

    "groups 3 series" in {
      grouper.series(L1.|>(3)) must beSome(OpenSerie(L1.|>(3), 6))
    }

    "groups rainbow series" in {
      grouper.series(List(L10, G10, B10)) must beSome(OpenSerie(List(L10, G10, B10), 30))
      grouper.series(Piece.<>(10)) must beSome(OpenSerie(Piece.<>(10), 40))
    }

    "dont group bad series" in {
      grouper.series(List(L1, L2, L4)) must beNone
      grouper.series(List(L1, L2, G3)) must beNone
    }

    "dont group 13 1 series" in {
      grouper.series(List(L10, L11, L12, L13, L1)) must beNone
    }

    "dont group bad rainbow series" in {
      grouper.series(List(L10, B10, L10)) must beNone
      grouper.series(List(L10, B10, R10, B10)) must beNone
      grouper.series(List(L10, B10, R10, G1)) must beNone
    }

    "dont group less than 3 series" in {
      grouper.series(List(L1, L2)) must beNone
      grouper.series(List(L1, G1)) must beNone
      grouper.series(List(L1)) must beNone
      grouper.series(Nil) must beNone
    }


    "groups pairs" in {
      grouper.pairs(L1.w) must beSome(OpenPair(L1.w, 1))
    }

    "dont group non pair" in {
      grouper.pairs(List(L1, L2)) must beNone
    }

    "dont group more than two" in {
      grouper.pairs(Nil) must beNone
      grouper.pairs(List(L1)) must beNone
      grouper.pairs(List(L1, L1, L1)) must beNone
    }

    // fake R3
    "group series with fake" in {
      grouper.series(List(R1, R2, F1)) must beSome(OpenSerie(List(R1, R2, F1), 6))
    }

    "group pairs with fake" in {
      grouper.pairs(List(R3, F1)) must beSome(OpenPair(List(R3, F1), 1))
      grouper.pairs(List(F1, F1)) must beSome(OpenPair(List(F1, F1), 1))
    }


    // okey R3
    "group series with okey only" in {
      grouper.series(List(G10, R3, G12)) must beSome(OpenSerie(List(G10, R3, G12), 33))
      grouper.series(List(R3, G11, G12)) must beSome(OpenSerie(List(R3, G11, G12), 33))
      grouper.series(List(G10, G11, R3)) must beSome(OpenSerie(List(G10, G11, R3), 33))


      grouper.series(List(G12, R3, G10)) must beSome(OpenSerie(List(G12, R3, G10), 33))
      grouper.series(List(G12, G11, R3)) must beSome(OpenSerie(List(G12, G11, R3), 33))
      grouper.series(List(R3, G11, G10)) must beSome(OpenSerie(List(R3, G11, G10), 33))
    }

    "group series with okey 4" in {
      grouper.series(List(G9, G10, R3, G12)) must beSome(OpenSerie(List(G9, G10, R3, G12), 42))
      grouper.series(List(G9, R3, G11, G12)) must beSome(OpenSerie(List(G9, R3, G11, G12), 42))
      grouper.series(List(G9, G10, G11, R3)) must beSome(OpenSerie(List(G9, G10, G11, R3), 42))

      grouper.series(List(G10, R3, G12, G13)) must beSome(OpenSerie(List(G10, R3, G12, G13), 46))
      grouper.series(List(R3, G11, G12, G13)) must beSome(OpenSerie(List(R3, G11, G12, G13), 46))
      grouper.series(List(G10, G11, R3, G13)) must beSome(OpenSerie(List(G10, G11, R3, G13), 46))


      grouper.series(List(G12, R3, G10, G9)) must beSome(OpenSerie(List(G12, R3, G10, G9), 42))
      grouper.series(List(G12, G11, R3, G9)) must beSome(OpenSerie(List(G12, G11, R3, G9), 42))
      grouper.series(List(R3, G11, G10, G9)) must beSome(OpenSerie(List(R3, G11, G10, G9), 42))
    }

    "group series with okey 5" in {
      grouper.series(List(G9, G10, R3, G12, G13)) must beSome(OpenSerie(List(G9, G10, R3, G12, G13), 55))
      grouper.series(List(G9, R3, G11, G12, G13)) must beSome(OpenSerie(List(G9, R3, G11, G12, G13), 55))
      grouper.series(List(G9, G10, G11, R3, G13)) must beSome(OpenSerie(List(G9, G10, G11, R3, G13), 55))

      grouper.series(List(G8, G9, G10, R3, G12)) must beSome(OpenSerie(List(G8, G9, G10, R3, G12), 50))
      grouper.series(List(G8, G9, R3, G11, G12)) must beSome(OpenSerie(List(G8, G9, R3, G11, G12), 50))
      grouper.series(List(G8, G9, G10, G11, R3)) must beSome(OpenSerie(List(G8, G9, G10, G11, R3), 50))
    }

    "group series with okey rainbow 3" in {
      grouper.series(List(G10, R3, B10)) must beSome(OpenSerie(List(G10, R3, B10), 30))
      grouper.series(List(L10, R3, B10)) must beSome(OpenSerie(List(L10, R3, B10), 30))
      grouper.series(List(R3, R10, B10)) must beSome(OpenSerie(List(R3, R10, B10), 30))
      grouper.series(List(R10, B10, R3)) must beSome(OpenSerie(List(R10, B10, R3), 30))
    }

    "group series with okey rainbow 4" in {
      grouper.series(List(L10, G10, R3, B10)) must beSome(OpenSerie(List(L10, G10, R3, B10), 40))
      grouper.series(List(G10, L10, R3, B10)) must beSome(OpenSerie(List(G10, L10, R3, B10), 40))
      grouper.series(List(G10, R3, R10, B10)) must beSome(OpenSerie(List(G10, R3, R10, B10), 40))
      grouper.series(List(G10, R10, B10, R3)) must beSome(OpenSerie(List(G10, R10, B10, R3), 40))
    }

    "group pairs" in {
      grouper.pairs(List(G10, R3)) must beSome(OpenPair(List(G10, R3), 1))
      grouper.pairs(List(R3, G10)) must beSome(OpenPair(List(R3, G10), 1))
    }


    "dont group bad series with okey" in {
      grouper.series(List(G10, R3, G9)) must beNone
      grouper.series(List(G10, R3, G10)) must beNone
      grouper.series(List(G10, R3, G11)) must beNone

      grouper.series(List(R3, G10, G10)) must beNone
      grouper.series(List(R3, G9, G7)) must beNone

      grouper.series(List(G10, G8, R3)) must beNone
    }

    "dont group bad rainbow with okey" in {
      grouper.series(List(G10, L1, R3)) must beNone
      grouper.series(List(G10, R3, L1)) must beNone
      grouper.series(List(R3, G10, L1)) must beNone

      grouper.series(List(R3, G10, L10, B1)) must beNone
      grouper.series(List(G10, R3, L10, B1)) must beNone
      grouper.series(List(G10, L10, R3, B1)) must beNone
      grouper.series(List(G10, L10, B1, R3)) must beNone
    }


    // fake R3
    "group fake with okey" in {
      grouper.series(List(R1, R3, F1)) must beSome(OpenSerie(List(R1, R3, F1), 6))
      grouper.series(List(B3, R3, F1)) must beSome(OpenSerie(List(B3, R3, F1), 9))
      grouper.series(List(F1, R3, R5)) must beSome(OpenSerie(List(F1, R3, R5), 12))

      grouper.pairs(List(F1, R3)) must beSome(OpenPair(List(F1, R3), 1))
    }

    "drops new piece on open serie" in {
      grouper.dropSeries(OpenSerie(List(G2, G3, G4), 9), G5, appendRight) must beSome(OpenSerie(List(G2, G3, G4, G5), 14))

      grouper.dropSeries(OpenSerie(List(G2, G3, G4), 9), G1, appendLeft) must beSome(OpenSerie(List(G1, G2, G3, G4), 10))
    }

    "drops new piece on open serie rainbow" in {
      grouper.dropSeries(OpenSerie(List(G2, R2, B2), 6), L2, appendLeft) must beSome(OpenSerie(List(L2, G2, R2, B2), 8))
    }

    // okey R3
    "drops okey on open serie" in {
      grouper.dropSeries(OpenSerie(List(G10, G11, G12), 33), R3, appendLeft) must beSome(OpenSerie(List(R3, G10, G11, G12), 42))

      grouper.dropSeries(OpenSerie(List(G10, G11, G12), 33), R3, appendRight) must beSome(OpenSerie(List(G10, G11, G12, R3), 46))
    }

    // fake R3
    "drops fake on open serie" in {
      grouper.dropSeries(OpenSerie(List(L3, G3, B3), 9), F1, appendRight) must beSome(OpenSerie(List(L3, G3, B3, F1), 12))

      grouper.dropSeries(OpenSerie(List(R4, R5, R6), 15), F1, appendLeft) must beSome(OpenSerie(List(F1, R4, R5, R6), 18))
    }

    "dont drop bad serie" in {
      grouper.dropSeries(OpenSerie(List(G10, G11, G12, G13), 33), R3, appendRight) must beNone

      grouper.dropSeries(OpenSerie(List(G1, L1, B1, R1), 4), L1, appendLeft) must beNone

      grouper.dropSeries(OpenSerie(List(G11, G12, G13), 35), R3, appendRight) must beNone
    }

    "replace okey on open serie" in {
      grouper.dropSeries(OpenSerie(List(L3, R3, G3), 9), B3, ReplaceOkey(1)) must beSome(OpenSerie(List(L3, B3, G3), 9))

      grouper.dropSeries(OpenSerie(List(R3, L11, L12), 33), L10, ReplaceOkey(1)) must beSome(OpenSerie(List(L10, L11, L12), 33))
    }

    "replace okey with fake" in {
      grouper.dropSeries(OpenSerie(List(L3, R3, G3), 9), F1, ReplaceOkey(1)) must beSome(OpenSerie(List(L3, F1, G3), 9))
    }

    "dont replace bad serie" in {
      grouper.dropSeries(OpenSerie(List(L3, R3, G3), 9), G4, ReplaceOkey(1)) must beNone

      grouper.dropSeries(OpenSerie(List(R3, L11, L12), 33), L13, ReplaceOkey(1)) must beNone
    }

    "replace okey on open pair" in {
      grouper.dropPairs(OpenPair(List(L3, R3), 1), L3) must beSome(OpenPair(List(L3, L3), 1))
      grouper.dropPairs(OpenPair(List(R3, G1), 1), G1) must beSome(OpenPair(List(G1, G1), 1))
    }

    "dont replace bad open pair" in {
      grouper.dropPairs(OpenPair(List(R3, G1), 1), G2) must beNone
    }
  }
}
