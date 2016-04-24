package okey
package variant

import scala.util.Random

abstract class Variant(
  val id: Int,
  val key: String,
  val name: String,
  val shortName: String,
  val title: String) {

  def dealer(side: Side): Dealer = StandardDealer(side)

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

sealed trait Dealer {
  val side: Side

  val pieces: List[Piece]
  val boards: Sides[Board]
  val sign: Piece
  val middles: List[Piece]
}

case class StandardDealer(side: Side) extends Dealer {

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

case class TestDealer(side: Side) extends Dealer {

  import Piece._

  def boardsCount = 21 * 4 + 1

  lazy val pieces: List[Piece] = Random.shuffle(Piece.initial)

  lazy val boards: Sides[Board] = dealBoards(21)

  lazy val sign: Piece = G13

  lazy val middles: List[Piece] = pieces drop (boardsCount + 1)

  private[variant] def dealBoards(count: Int): Sides[Board] = {
    def withExtra(ps: List[Piece], s: Side):List[Piece] =
      (s == side) fold((pieces head) :: ps, ps)

    val extra = List(G1, L8)
    val east = List(extra, R1.w, R2.w, R3.w, R4.w, R5.w).flatten
    val west = List(extra, Piece.<>(10), Piece.<>(10), Piece.<>(5), R1.|>(3)).flatten
    val north = List(extra, Piece.<>(10), Piece.<>(10), Piece.<>(8)).flatten
    val south = List(extra, L1.w, L2.w, L3.w, L4.w, L5.w, L6.w).flatten

    Sides(
      Board(withExtra(east, EastSide)),
      Board(withExtra(west, WestSide)),
      Board(withExtra(north, NorthSide)),
      Board(withExtra(south, SouthSide)))
  }
}

object Variant {
  val default = Standard
  val test = StandardTest

  val all = List(Standard)
  val byId = all map { v => (v.id, v) } toMap
  val byKey = all map { v => (v.key, v) } toMap

  def apply(id: Int): Option[Variant] = byId get id
  def orDefault(id: Int): Variant = apply(id) | default
}
