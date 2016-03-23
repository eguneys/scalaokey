package okey

import Piece._

import okey.variant.{ StandardScoringSystem }

class ScoreTest extends OkeyTest {

  "scoring system" should {
    import ScoringSystem._

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

      system.handSum(situation, EastSide) must_== 6
      system.handSum(situation, WestSide) must_== 24
      system.handSum(situation, NorthSide) must_== 2
      system.handSum(situation, SouthSide) must_== 0
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
  }

}
