package okey

import Piece._

class ClockTest extends OkeyTest {

  "play with a clock" should {

    val clock = Clock(5 * 60 * 1000)
    val game = Game(
      makeTable(R1, R2, R3, R4, R13, B1),
      Player(EastSide)) withClock clock.start

    "new game" in {
      game.clock must beSome.like {
        case c => c.side must_== EastSide
      }
    }
    "one move played" in {
      game.playMoves(EastSide, DrawMiddle) must beSuccess.like {
        case g => g.clock must beSome.like {
          case c => c.side must_== EastSide
        }
      }
    }

    "one turn played" in {
      game.playMoves(EastSide, DrawMiddle, Discard(R1)) must beSuccess.like {
        case g => g.clock must beSome.like {
          case c => c.side must_== NorthSide
        }
      }
    }
  }

  "create a clock" should {
    "with time" in {
      Clock(60).limit must_== 60
    }
  }

  "clock step" should {
    def clockStep(wait: Float): Double = {
      val clock = Clock(60).start.step
      Thread sleep (wait * 1000).toInt
        (clock.step remainingTime NorthSide).toDouble
    }
    def clockStart: Double = {
      val clock = Clock(60).start.step
      (clock.step remainingTime EastSide).toDouble
    }
    val delta = 0.2
    "move, nolag" in {
      clockStep(0) must beCloseTo(60, delta)
    }

    "1smove" in {
      clockStep(1f) must beCloseTo(59, delta)
    }

    "start nolag" in {
      clockStart must beCloseTo(60, delta)
    }
  }
}
