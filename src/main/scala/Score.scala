package okey

trait Score {
  def value: Int
}

case class ScoreSheet(scores: List[Score]) {
  val total = scores.foldLeft(0)(_ + _.value)
}

object ScoreSheet {
  val emptySheet = ScoreSheet(Nil)
}

case class EndScoreSheet(handSum: Int, scores: List[FlagScore]) {
  val total = scores.foldRight(handSum: Int) { _ apply _ }
}

sealed abstract class Flag(val id: Int)
sealed abstract class FlagScore(val id: Int) {
  val flag: Flag
  def apply(acc: Int): Int
}

abstract class ScoringSystem {

  import ScoringSystem._

  def scorer(flag: Flag): FlagScore

  def flags(situation: Situation, side: Side): List[Flag] = allFlags filter {
    case EndByHand => situation.handEnd
    case EndByPair => situation.pairEnd
    case EndByDiscardOkey => situation.okeyEnd
    case HandZero => situation.table.boards(side).isEmpty
    case HandOkey => situation.table.boards(side).exists(situation.table.okey)
    case HandOpenPair => situation.openStates(side) exists (_.pairs)
    case HandOpenNone => !situation.openStates(side).isDefined
  }

  def handSum(situation: Situation, side: Side): Int = situation.table.boards(side).handSum

  def sheet(situation: Situation, side: Side) = {
    val sum = handSum(situation, side)
    val scores = flags(situation, side) map { f => scorer(f) }

    EndScoreSheet(sum, scores)
  }
}

object ScoringSystem {

  case class Adder(val value: Int, flag: Flag) extends FlagScore(1) {
    def apply(acc: Int) = acc + value
  }
  case class Double(flag: Flag) extends FlagScore(2) {
    def apply(acc: Int) = acc * 2
  }


  case object EndByHand extends Flag(1)
  case object EndByPair extends Flag(2)
  case object EndByDiscardOkey extends Flag(3)

  case object HandZero extends Flag(4)
  case object HandOkey extends Flag(5)
  case object HandOpenNone extends Flag(6)
  case object HandOpenPair extends Flag(7)

  val allFlags = List(EndByHand, EndByPair, EndByDiscardOkey, HandZero, HandOkey, HandOpenNone, HandOpenPair)
}
