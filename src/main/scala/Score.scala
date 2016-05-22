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

trait EndScoreSheet {

  val handSum: Int
  val scores: Map[Flag, Option[FlagScore]]

  val total: Int

  // import FlagScore._

  // val total: Int = {
  //   val (sum, mult) = scores.foldRight((0, 1)) {
  //     case ((f, Some(HandSum)), (s, m)) => (s + handSum, m)
  //     case ((f, Some(Penalty)), (s, m)) => (s + 101, m)
  //     case ((f, Some(Erase)), (s, m)) => (s - 101, m)
  //     case ((f, Some(Double)), (s, m)) => (s, m * 2)
  //     case (_, (s, m)) => (s, m)
  //   }
  //   sum * mult
  // }
}

object EndScoreSheet {
  def byVariant(v: okey.variant.Variant) = v match {
    case okey.variant.DuzOkey => okey.variant.DuzOkeyScoringSystem.EndScoreSheet
    case okey.variant.DuzOkeyTest => okey.variant.DuzOkeyScoringSystem.EndScoreSheet
    case _ => okey.variant.StandardScoringSystem.EndScoreSheet
  }
}

sealed abstract class Flag(val id: Int)

sealed abstract class FlagScore(val id: Int)

abstract class ScoringSystem {

  import ScoringSystem._

  def handSum(situation: Situation, side: Side) = 
    situation.table.handSum(side)

  def scores(situation:Situation, side: Side) = {
    val fs = flags(situation, side)
    fs map { f => f -> scorer(f, fs) } toMap
  }

  def scorer(flag: Flag, flags: List[Flag]): Option[FlagScore]

  def flags(situation: Situation, side: Side): List[Flag] = allFlags filter {
    case EndByHand => situation.handEnd
    case EndByPair => situation.pairEnd
    case EndByDiscardOkey => situation.okeyEnd
    case HandZero => situation.table.boards(side).isEmpty
    case HandOkey => situation.table.boards(side).exists(situation.table.okey)
    case HandOpenPair => situation.openStates(side) exists (_.pairs)
    case HandOpenNone => !situation.openStates(side).isDefined
    case HandOpenSome => situation.openStates(side).isDefined
  }

  def sheet(situation: Situation, side: Side): EndScoreSheet
}

object FlagScore {
  case object HandSum extends FlagScore(4)
  case object Penalty extends FlagScore(1)
  case object Erase extends FlagScore(2)
  case object Double extends FlagScore(3)

  def apply(id: Int): Option[FlagScore] = allIds get id

  val allFlagScores: List[FlagScore] = List(HandSum, Penalty, Erase, Double)

  val allIds = allFlagScores map { f => f.id -> f } toMap
}

object Flag {
  def apply(id: String): Option[Flag] =  parseIntOption(id) flatMap ScoringSystem.flagById
}

object ScoringSystem {

  case object EndByHand extends Flag(1)
  case object EndByPair extends Flag(2)
  case object EndByDiscardOkey extends Flag(3)

  case object HandZero extends Flag(4)
  case object HandOkey extends Flag(5)
  case object HandOpenNone extends Flag(6)
  case object HandOpenPair extends Flag(7)
  case object HandOpenSome extends Flag(8)

  def flagById(id: Int): Option[Flag] = allFlagIds get id

  val allFlags = List(HandOpenSome, EndByHand, EndByPair, EndByDiscardOkey, HandZero, HandOkey, HandOpenNone, HandOpenPair)

  val allFlagIds = allFlags map { f => f.id -> f } toMap
}
