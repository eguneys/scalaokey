package okey
package variant

import scala.util.Random
import okey.{ ScoringSystem => AbstractScoringSystem }

case object DuzOkey extends Variant(
  id = 2,
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

case class DuzOkeyDealer(side: Side) extends Dealer {

  def nbEach = 14

  def nbBoards = nbEach * 4 + 1

  lazy val pieces: List[Piece] = Random.shuffle(Piece.initial)

  lazy val boards: Sides[Board] = dealBoards(nbEach)

  lazy val sign: Piece = pieces(nbBoards)

  lazy val middles: List[Piece] = pieces drop (nbBoards + 1)

  private[variant] def dealBoards(count: Int): Sides[Board] = {
    def withExtra(ps: List[Piece], s: Side):List[Piece] =
      (s == side) fold((pieces head) :: ps, ps)

    val deals = (pieces drop 1) grouped count

    Sides(
      Board(withExtra(deals.next, EastSide)),
      Board(withExtra(deals next, WestSide)),
      Board(withExtra(deals next, NorthSide)),
      Board(withExtra(deals next, SouthSide)))
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
