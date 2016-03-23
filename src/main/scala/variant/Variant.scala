package okey
package variant

import scala.util.Random

abstract class Variant(
  val id: Int,
  val name: String) {

  val scoringSystem: ScoringSystem

  def validMoves(situation: Situation): List[Action] = situation.actor.moves

  def move(situation: Situation, side: Side, action: Action): Valid[Move] = {
    def findMove(action: Action): Option[Move] = situation.toMove(action)

    val actor = situation.actor
    for {
      myActor <- actor.validIf(actor is side, "Not my turn of " + side)
      m1 <- findMove(action) toValid "Not a valid move " + action
    } yield m1
  }

  def winner(situation: Situation): Option[Side] = None

  def endScores(situation: Situation): Option[Sides[EndScoreSheet]] = situation.end option Sides(scoringSystem.sheet(situation, _))

  def finalizeTable(table: Table, player: Player, action: Action): Table = table
}

case class Dealer(side: Side) {

  def boardsCount = 21 * 4 + 1

  lazy val pieces: List[Piece] = Random.shuffle(Piece.initial)

  lazy val boards: Sides[Board] = dealBoards(21)

  lazy val sign: Piece = pieces(boardsCount)

  lazy val middles: List[Piece] = pieces drop (boardsCount + 1)

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

object Variant {
  val default = Standard
}
