package okey
package variant

import scala.util.Random
import okey.{ ScoringSystem => AbstractScoringSystem }

case object DuzOkey extends Variant(
  id = 3,
  key = "duzokey",
  name = "Düz Okey",
  shortName ="Düz Okey",
  title = "Elini diz, gösterge yap, taşını at, kazan.") {

  val scoringSystem = DuzOkeyScoringSystem

  override def hasOpener = false

  override def specialEnd(situation: Situation) =
    situation.duzNormalEnd || situation.duzPairEnd

  override def dealer(side: Side): Dealer = DuzOkeyDealer(side)

}

case object DuzOkeyTest extends Variant(
  id = 4,
  key = "duzokeytest",
  name = "Düz Okey Test",
  shortName ="Düz Okey Test",
  title = "Elini diz, gösterge yap, taşını at, kazan.") {

  val scoringSystem = DuzOkeyScoringSystem

  override def hasOpener = false

  override def specialEnd(situation: Situation) =
    situation.duzNormalEnd || situation.duzPairEnd

  override def dealer(side: Side): Dealer = DuzOkeyDealerTest(side)

}

case class DuzOkeyDealer(side: Side) extends Dealer {
  val nbEach = 14
}

case class DuzOkeyDealerTest(side: Side) extends Dealer {
  import Piece._

  val nbEach = 14

  override lazy val sign: Piece = G13

  override def dealBoards(count: Int): Sides[Board] = {
    def withExtra(ps: List[Piece], s: Side):List[Piece] =
      (s == side) fold((pieces head) :: ps, ps)

    val extra = List(G1, L8)
    val east = List(extra, R1.w, R2.w, R3.w, R4.w, R5.w, R6.w).flatten
    val west = List(extra, R1.|>(6), R13.<|(6)).flatten
    val north = List(extra, Piece.<>(10), Piece.<>(10), Piece.<>(5)).flatten
    val south = List(extra, Piece.<>(10), Piece.<>(10), Piece.<>(8)).flatten


    Sides(
      Board(withExtra(east, EastSide)),
      Board(withExtra(west, WestSide)),
      Board(withExtra(north, NorthSide)),
      Board(withExtra(south, SouthSide)))
  }
}

object DuzOkeyScoringSystem extends AbstractScoringSystem {

  import AbstractScoringSystem._
  import FlagScore._

  override def flags(situation: Situation, side: Side): List[Flag] = allFlags filter {
    case EndByPair => situation.duzPairEnd
    case EndByDiscardOkey => situation.duzOkeyEnd
    case _ => false
  }

  def scorer(flag: Flag, flags: List[Flag]): Option[FlagScore] = flag match {
    case EndByPair => Penalty.some
    case EndByDiscardOkey => Double.some
    case _ => None

  }
}
