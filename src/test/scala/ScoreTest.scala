package okey

import Piece._

import okey.variant.{ StandardScoringSystem }
import FlagScore._

class ScoreTest extends OkeyTest {

  "scoring system" should {
    import ScoringSystem._
    import StandardScoringSystem._

    val system = StandardScoringSystem

    // previous player is east
    val player = Player(side = NorthSide, drawMiddle = true)

    val emptyHistory = History()
    val discardHistory = History(DrawMiddle, Discard(G1))

    val emptyOpens = Sides[Option[Opens]]

    def makeOpens(from: Sides[Option[Opens]], side: Side, pairs: Boolean, old: Boolean) = from.withSide(side, Opens(pairs, old).some)

    val westOpens = makeOpens(emptyOpens, WestSide, old = true, pairs = false)

    def eastOpens(from: Sides[Option[Opens]] = emptyOpens, old: Boolean, pairs: Boolean) = makeOpens(from, EastSide, old = old, pairs = pairs)

    val emptyEastSituation = ("""
r13
r1r2

g1
""" as player)

    val fullEastSituation = ("""
r13
r1r2
r1
g1
""" as player)

    "score flags" in {
      "normal" in {
        val flags = Nil
        system.scorer(EndByHand, flags) must_== Double.some
        system.scorer(EndByPair, flags) must_== Double.some
        system.scorer(EndByDiscardOkey, flags) must_== Double.some

        system.scorer(HandZero, flags) must_== Erase.some
        system.scorer(HandOpenNone, flags) must_== Penalty.some
        system.scorer(HandOpenPair, flags) must_== Double.some
      }

      "hand okey" in {
        "no penalty if not opened" in {
        val flags = List(HandOkey, HandOpenNone)
        system.scorer(HandOkey, flags) must beNone
        }
        "penalty if opened" in {
          system.scorer(HandOkey, List(HandOpenSome)) must_== Penalty.some
        }
      }
    }

    "hand sum" in {
      val situation = ("""
r13
r1r2
r1r2r3
g1g10g13
l2
""" as player)

      "sums hands" in {
        system.handSum(situation, WestSide) must_== 24
        system.handSum(situation, NorthSide) must_== 2
        system.handSum(situation, SouthSide) must_== 0
      }

      "ignores okey" in {
        system.handSum(situation, EastSide) must_== 5 // 6
      }

      "sums fake" in {
        3 must_== 4
      }.pendingUntilFixed
    
    }

    "find flags" in {
      "end by hand" in {
        val hist = discardHistory.withOpenStates(eastOpens(old = false, pairs = false))

        val situation = emptyEastSituation.withHistory(hist)
        system.flags(situation, EastSide) must_== List(HandOpenSome, EndByHand, HandZero)
      }

      "end by pair" in {
        val hist = discardHistory.withOpenStates(eastOpens(old = false, pairs = true))

        val situation = emptyEastSituation.withHistory(hist)
        system.flags(situation, EastSide) must_== List(HandOpenSome, EndByHand, EndByPair, HandZero, HandOpenPair)
      }

      "end by discard okey" in {
        val hist = History(DrawMiddle, Discard(R1)).withOpenStates(eastOpens(old = true, pairs = false))

        val situation = emptyEastSituation.withHistory(hist)
        system.flags(situation, EastSide) must_== List(HandOpenSome, EndByDiscardOkey, HandZero)
      }

      "hand zero" in {
        val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = true, pairs = false))

        val situation = emptyEastSituation.withHistory(hist)
        system.flags(situation, EastSide) must_== List(HandOpenSome, HandZero)
        system.flags(situation, WestSide) must_== List(HandOpenSome)
      }

      "hand okey" in {
        val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = false, pairs = true))

        val situation = ("""
r13
r1r2

g1r1
r13
""" as player).withHistory(hist)
        system.flags(situation, EastSide) must_== List(HandOpenSome, EndByPair, HandZero, HandOpenPair)
        system.flags(situation, WestSide) must_== List(HandOpenSome, EndByPair, HandOkey)
        system.flags(situation, NorthSide) must_== List(EndByPair, HandOpenNone)
      }

      "hand open pair" in {
        val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = true, pairs = true))

        val situation = emptyEastSituation.withHistory(hist)
        system.flags(situation, EastSide) must_== List(HandOpenSome, EndByPair, HandZero, HandOpenPair)
        system.flags(situation, WestSide) must_== List(HandOpenSome, EndByPair)
      }

      "hand open none" in {
        val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = true, pairs = false))

        val situation = emptyEastSituation.withHistory(hist)
        system.flags(situation, EastSide) must_== List(HandOpenSome, HandZero)
        system.flags(situation, WestSide) must_== List(HandOpenSome)
        system.flags(situation, NorthSide) must_== List(HandZero, HandOpenNone)
      }
    }


    "evaluate score sheet" should {
      def makeSheet(handSum: Int, flags: Flag*): EndScoreSheet =
        EndScoreSheet(handSum, flags map (f => f -> system.scorer(f, flags toList)) toMap)


      def makeSheetWithOpen(handSum: Int, flags: Flag*): EndScoreSheet =
        makeSheet(handSum, HandOpenSome :: flags.toList :_*)

      "hand open series" in {
        //"gets hand sum" in { makeSheet(11).total must_== 11 }
        "gets hand sum" in { makeSheetWithOpen(11).total must_== 11 }

        "hand okey gets hand sum +101" in {
          makeSheetWithOpen(11, HandOkey).total must_== 112
        }

        "hand zero gets -101" in {
          makeSheetWithOpen(0, HandOpenSome, HandZero).total must_== -101
          makeSheetWithOpen(0, HandOpenSome, EndByDiscardOkey, HandZero).total must_== -202
          makeSheetWithOpen(0, HandOpenSome, EndByHand, EndByDiscardOkey, HandZero).total must_== -404
        }

        "end by discard/hand gets double score" in {
          makeSheetWithOpen(11, EndByDiscardOkey).total must_== 22
          makeSheetWithOpen(11, EndByHand).total must_== 22
        }
      }

      "hand pair gets double hand sum" in {
        makeSheetWithOpen(11, HandOpenPair).total must_== 22
      }

      // maybe gets -202?
      "hand zero and hand open pair gets -101" in {
        makeSheetWithOpen(0, HandZero, EndByPair, HandOpenPair).total must_== -101
      }.pendingUntilFixed

      "no open" in {
        "gets 101 ignoring handsum" in {
          makeSheet(11, HandOpenNone).total must_== 101
        }

        "hand okey gets 101 ignoring penalty" in {
          makeSheet(11, HandOpenNone, HandOkey).total must_== 101
        }

        "end by discard/hand gets double" in {
          makeSheet(11, EndByDiscardOkey, HandOpenNone).total must_== 202
          makeSheet(11, EndByHand, EndByDiscardOkey, HandOpenNone).total must_== 404
        }
      }
    }
  }
}
