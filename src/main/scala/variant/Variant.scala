package okey
package variant

import scala.util.Random

abstract class Variant(
  val id: Int,
  val name: String) {

  def validMoves(situation: Situation): List[Action] = situation.actor.moves

  def end(situation: Situation): Boolean = false

  def move(situation: Situation, side: Side, action: Action): Valid[Move] = {
    def findMove(action: Action): Option[Move] = situation.toMove(action)

    val actor = situation.actor
    for {
      myActor <- actor.validIf(actor is side, "Not my turn of " + side)
      m1 <- findMove(action) toValid "Not a valid move " + action
    } yield m1
  }

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
