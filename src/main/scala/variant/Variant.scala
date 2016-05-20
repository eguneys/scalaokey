package okey
package variant

import scala.util.Random

abstract class Variant(
  val id: Int,
  val key: String,
  val name: String,
  val shortName: String,
  val title: String) {

  def standard = this == Standard
  def yuzbir = this == Standard || this == StandardTest

  def exotic = !standard

  def hasOpener: Boolean = true

  def specialEnd(situation: Situation): Boolean = false

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

  def winner(situation: Situation): Option[Side] =
    if (situation.middleEnd) None
    else
      situation.endScores map { scores =>
        (scores zip Side.all).reduce[(EndScoreSheet, Side)] {
          case (s1, s2) if s1._1.total < s2._1.total => s1
          case (_, s2) => s2
        }._2
      }

  def endStanding(situation: Situation): Option[Sides[Int]] = situation.endScores map Variant.endStanding

  def endScores(situation: Situation): Option[Sides[EndScoreSheet]] = situation.end option Sides(scoringSystem.sheet(situation, _))

  def finalizeTable(table: Table, player: Player, action: Action): Table = table
}

// TODO FIXME dont allow fake okey to be sign
trait Dealer {

  val nbEach: Int

  def nbBoards = nbEach * 4 + 1

  lazy val pieces: List[Piece] = Random.shuffle(Piece.initial)

  lazy val sign: Piece = pieces(nbBoards)

  lazy val middles: List[Piece] = pieces drop (nbBoards + 1)

  lazy val boards: Sides[Board] = dealBoards(nbEach)

  def dealBoards(count: Int): Sides[Board] = {
    def withExtra(ps: List[Piece], s: Side):List[Piece] =
      (s == side) fold((pieces head) :: ps, ps)

    val deals = (pieces drop 1) grouped count

    Sides(
      Board(withExtra(deals.next, EastSide)),
      Board(withExtra(deals next, WestSide)),
      Board(withExtra(deals next, NorthSide)),
      Board(withExtra(deals next, SouthSide)))
  }

  val side: Side
}

case class StandardDealer(side: Side) extends Dealer {
  val nbEach = 21
}

case class TestDealer(side: Side) extends Dealer {

  val nbEach = 21

  import Piece._

  override lazy val sign: Piece = G13

  override def dealBoards(count: Int): Sides[Board] = {
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

  val all = List(Standard, DuzOkey, StandardTest, DuzOkeyTest)
  val byId = all map { v => (v.id, v) } toMap
  val byKey = all map { v => (v.key, v) } toMap

  def apply(id: Int): Option[Variant] = byId get id
  def orDefault(id: Int): Variant = apply(id) | default


  def endStanding(scores: Sides[EndScoreSheet]): Sides[Int] = {
    val totals = scores map (_.total)
    Side.all map { side =>
      { totals count (_ < scores(side).total) } + 1
    }
  }
}
