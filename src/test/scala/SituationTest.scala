package okey

import Piece._

class SituationTest extends OkeyTest {

  "a game" should {

    "detect end" should {
      // previous player is east
      val player = Player(side = NorthSide, drawMiddle = true)

      val emptyHistory = History()
      val discardHistory = History(DrawMiddle, Discard(G1))

      val emptyOpens = Sides[Option[Opens]]

      def makeOpens(from: Sides[Option[Opens]], side: Side, pairs: Boolean, old: Boolean) = from.withSide(side, Opens(pairs, old).some)

      def eastOpens(from: Sides[Option[Opens]] = emptyOpens, old: Boolean, pairs: Boolean) = makeOpens(from, EastSide, old = old, pairs = pairs)

      val emptyEastSituation = ("""
r13
r1r2
""" as player)

      val fullEastSituation = ("""
r13
r1r2
r1
""" as player)

      "by middle" in {

        ("""
r13
r1
""" as player).middleEnd must beFalse

        "dont end before discard" in {
          ("""
r13
""" as player).middleEnd must beFalse
        }

        ("""
r13
""" as player withHistory discardHistory).middleEnd must beTrue

      }


      "by normal" in {
        "no discard" in {
          emptyEastSituation.normalEnd must beFalse
          fullEastSituation.normalEnd must beFalse
        }

        "with discard" in {
          "if player board has no pieces" in {
            emptyEastSituation.withHistory(discardHistory).normalEnd must beTrue
          }

          "if player board has pieces" in {
            fullEastSituation.normalEnd must beFalse
          }
        }
      }

      "find winner" in {
        "no winner if middle end" in {
          val situation = ("""
r13
""" as player withHistory discardHistory)

          situation.winner must beNone

        }
      }

      "find standings" in {
        import okey.variant.Variant
        import ScoringSystem._

        val system = okey.variant.StandardScoringSystem
        def makeSheet(handSum: Int, flags: Flag*): EndScoreSheet =
          system.EndScoreSheet(handSum, flags map (f => f -> system.scorer(f, flags toList)) toMap)

        "all different" in {
          val scores = Sides(
            makeSheet(0, HandOpenSome),
            makeSheet(10, HandOpenSome),
            makeSheet(50,  HandOpenSome),
            makeSheet(40, HandOpenSome))

          Variant.endStanding(scores) must_== Sides(1, 2, 4, 3)
        }

        "same" in {
          val scores = Sides(
            makeSheet(0, HandOpenSome),
            makeSheet(0, HandOpenSome),
            makeSheet(50,  HandOpenSome),
            makeSheet(40, HandOpenSome))

          Variant.endStanding(scores) must_== Sides(1, 1, 4, 3)
        }
      }

      "by discard okey" in {
        val situationR13 = ("""
r13
r1r2
""" as player)
        "with discard with other piece" in {
          val history = History(DrawMiddle, Discard(G1))
          situationR13.withHistory(history).okeyEnd must beFalse
        }

        "with discard with sign piece" in {
          val history = History(DrawMiddle, Discard(R13))
          situationR13.withHistory(history).okeyEnd must beFalse
        }

        "with discard with okey piece" in {
          val history = History(DrawMiddle, Discard(R1))
          situationR13.withHistory(history).okeyEnd must beTrue
        }
      }

      "by hand and by pair" should {

        "if nobody opened" should {
          "east not opened" in {
            val hist = discardHistory.withOpenStates(emptyOpens)
            fullEastSituation.withHistory(hist).normalEnd must beFalse
          }

          "east opened series" in {
            "old open" in {
              val hist = discardHistory.withOpenStates(eastOpens(old = true, pairs = false))
              emptyEastSituation.withHistory(hist).handEnd must beFalse
              emptyEastSituation.withHistory(hist).pairEnd must beFalse
            }
            "new open" in {
              val hist = discardHistory.withOpenStates(eastOpens(old = false, pairs = false))
              emptyEastSituation.withHistory(hist).handEnd must beTrue
              emptyEastSituation.withHistory(hist).pairEnd must beFalse
            }
          }

          "east opened pairs" in {
            "old open" in {
              val hist = discardHistory.withOpenStates(eastOpens(old = true, pairs = true))
              emptyEastSituation.withHistory(hist).handEnd must beFalse
              emptyEastSituation.withHistory(hist).pairEnd must beTrue
            }
            "new open" in {
              val hist = discardHistory.withOpenStates(eastOpens(old = false, pairs = true))
              emptyEastSituation.withHistory(hist).handEnd must beTrue
              emptyEastSituation.withHistory(hist).pairEnd must beTrue
            }
          }
        }

        "if west opened" should {
          "west opened series" in {
            val westOpens = makeOpens(emptyOpens, WestSide, old = true, pairs = false)
            "east not opened" in {
              val hist = discardHistory.withOpenStates(westOpens)
              emptyEastSituation.withHistory(hist).handEnd must beFalse
              emptyEastSituation.withHistory(hist).pairEnd must beFalse
            }

            "east opened series" should {
              "old open" in {
                val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = true, pairs = false))
                emptyEastSituation.withHistory(hist).handEnd must beFalse
                emptyEastSituation.withHistory(hist).pairEnd must beFalse

              }
              "new open" in {
                val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = false, pairs = false))
                emptyEastSituation.withHistory(hist).handEnd must beFalse

                emptyEastSituation.withHistory(hist).pairEnd must beFalse
              }
            }

            "east opened pairs" in {
              "old open" in {
                val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = true, pairs = true))
                emptyEastSituation.withHistory(hist).handEnd must beFalse
                emptyEastSituation.withHistory(hist).pairEnd must beTrue
              }
              "new open" in {
                val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = false, pairs = true))
                emptyEastSituation.withHistory(hist).handEnd must beFalse
                emptyEastSituation.withHistory(hist).pairEnd must beTrue
              }
            }
          }

          "west opened pairs" in {
            val westOpens = makeOpens(emptyOpens, WestSide, old = true, pairs = true)
            "east not opened" in {
              val hist = discardHistory.withOpenStates(westOpens)
              emptyEastSituation.withHistory(hist).handEnd must beFalse
              emptyEastSituation.withHistory(hist).pairEnd must beFalse
            }

            "east opened series" in {
              "old open" in {
                val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = true, pairs = false))
                emptyEastSituation.withHistory(hist).handEnd must beFalse
                emptyEastSituation.withHistory(hist).pairEnd must beFalse
              }
              "new open" in {
                val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = false, pairs = false))
                emptyEastSituation.withHistory(hist).handEnd must beFalse
                emptyEastSituation.withHistory(hist).pairEnd must beFalse
              }
            }

            "east opened pairs" in {
              "old open" in {
                val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = true, pairs = true))
                emptyEastSituation.withHistory(hist).handEnd must beFalse
                emptyEastSituation.withHistory(hist).pairEnd must beTrue
              }
              "new open" in {
                val hist = discardHistory.withOpenStates(eastOpens(westOpens, old = false, pairs = true))
                emptyEastSituation.withHistory(hist).handEnd must beFalse
                emptyEastSituation.withHistory(hist).pairEnd must beTrue
              }
            }
          }
        }
      }
    }
  }
}
