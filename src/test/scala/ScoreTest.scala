package okey

import Piece._

import okey.variant.{ StandardScoringSystem }

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
      system.scorer(EndByHand) must_== Double(EndByHand)
      system.scorer(EndByPair) must_== Double(EndByPair)
      system.scorer(EndByDiscardOkey) must_== Double(EndByDiscardOkey)

      system.scorer(HandZero) must_== Adder(-101, HandZero)
      system.scorer(HandOkey) must_== Adder(101, HandOkey)
      system.scorer(HandOpenNone) must_== Adder(101, HandOpenNone)
      system.scorer(HandOpenPair) must_== Double(HandOpenPair)
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
        system.flags(situation, EastSide) must_== List(EndByHand, HandZero)
      }

      "end by pair" in {
        val hist = discardHistory.withOpenStates(eastOpens(old = false, pairs = true))

        val situation = emptyEastSituation.withHistory(hist)
        system.flags(situation, EastSide) must_== List(EndByHand, EndByPair, HandZero, HandOpenPair)
      }

      "end by discard okey" in {
        val hist = History(DrawMiddle, Discard(R1)).withOpenStates(eastOpens(old = true, pairs = false))

        val situation = emptyEastSituation.withHistory(hist)
        system.flags(situation, EastSide) must_== List(EndByDiscardOkey, HandZero)
      }

      "hand zero" in {
        val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = true, pairs = false))

        val situation = emptyEastSituation.withHistory(hist)
        system.flags(situation, EastSide) must_== List(HandZero)
        system.flags(situation, WestSide) must_== List()
      }

      "hand okey" in {
        val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = false, pairs = true))

        val situation = ("""
r13
r1r2

g1r1
r13
""" as player).withHistory(hist)
        system.flags(situation, EastSide) must_== List(EndByPair, HandZero, HandOpenPair)
        system.flags(situation, WestSide) must_== List(EndByPair, HandOkey)
        system.flags(situation, NorthSide) must_== List(EndByPair, HandOpenNone)
      }

      "hand open pair" in {
        val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = true, pairs = true))

        val situation = emptyEastSituation.withHistory(hist)
        system.flags(situation, EastSide) must_== List(EndByPair, HandZero, HandOpenPair)
        system.flags(situation, WestSide) must_== List(EndByPair)
      }

      "hand open none" in {
        val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = true, pairs = false))

        val situation = emptyEastSituation.withHistory(hist)
        system.flags(situation, EastSide) must_== List(HandZero)
        system.flags(situation, WestSide) must_== List()
        system.flags(situation, NorthSide) must_== List(HandZero, HandOpenNone)
      }
    }


    "evaluate score sheet" should {
      def makeSheet(handSum: Int, flags: Flag*): Sheet =
        Sheet(handSum, flags map system.scorer toList)

      "hand open series" in {
        "gets hand sum" in { makeSheet(11).total must_== 11 }

        "hand okey gets hand sum +101" in {
          makeSheet(11, HandOkey).total must_== 112
        }

        "hand zero gets -101" in {
          makeSheet(0, HandZero).total must_== -101
          makeSheet(0, EndByDiscardOkey, HandZero).total must_== -202
          makeSheet(0, EndByHand, EndByDiscardOkey, HandZero).total must_== -404
        }

        "end by discard/hand gets double score" in {
          makeSheet(11, EndByDiscardOkey).total must_== 22
          makeSheet(11, EndByHand).total must_== 22
        }
      }

      "hand pair gets double hand sum" in {
        makeSheet(11, HandOpenPair).total must_== 22
      }

      // maybe gets -202?
      "hand zero and hand open pair gets -101" in {
        makeSheet(0, HandZero, EndByPair, HandOpenPair).total must_== -101
      }

      "no open" in {
        "gets 101 ignoring handsum" in {
          makeSheet(11, HandOpenNone).total must_== 101
        }

        "hand okey gets 101 ignoring double" in {
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
