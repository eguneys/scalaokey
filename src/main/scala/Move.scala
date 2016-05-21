package okey

import format.Uci

case class Move(
  player: Player,
  action: Action,
  before: Table,
  after: Table) {

  def finalizeAfter: Table = {
    val table = after

    val t1 = table.variant.finalizeTable(table, player, action)

    // commit open
    action match {
      case Discard(_) => t1.updateOpener(o => o.commitOpen(player.side) getOrElse o)
      case _ => t1
    }
  }

  def finalizePlayer: Player = {
    player(action) updateHistory { h1 =>
      action match {
        case dm@DrawMiddle(p) => h1.withLastMove(dm)
        case dl@DrawLeft(p) => h1.withLastMove(dl)
        case dd@Discard(p) => h1.addLastMove(dd).withOpenStates(after.opener).addTurns
        case a => h1.addLastMove(a)
      }
    }

    // add last
  }
}

object Move {

}

sealed trait Action {
  def toSingle: Action = this

  def withPiece(piece: Piece): Action = this

  def withPieceGroup(group: PieceGroups): Action = this

  def withDrop(piece: Piece, pos: OpenPos): Action = this

  val key: String = toSingle.key

  def toUci: Uci = Uci.Move(this)
}

case object DrawLeft extends Action {
  override val key = "dl"

  override def withPiece(piece: Piece) = DrawLeft(piece)
}

case object DrawMiddle extends Action {
  override val key = "dm"
}

case object Discard extends Action {
  override val key = "dd"

  override def withPiece(piece: Piece) = Discard(piece)
}

case object DiscardEndSeries extends Action {
  def apply(pieces: List[Piece]*): DiscardEndSeries = DiscardEndSeries(pieces toList)

  override val key = "dds"

  override def withPieceGroup(group: PieceGroups) = DiscardEndSeries(group)
}

case object DiscardEndPairs extends Action {
  def apply(pieces: List[Piece]*): DiscardEndPairs = DiscardEndPairs(pieces toList)

  override val key = "ddp"

  override def withPieceGroup(group: PieceGroups) = DiscardEndPairs(group)
}


case object ShowSign extends Action {
  override val key = "ss"

  override def withPiece(piece: Piece) = ShowSign(piece)
}

case object OpenSeries extends Action {
  def apply(pieces: List[Piece]*): OpenSeries = OpenSeries(pieces toList)

  override val key = "os"

  override def withPieceGroup(group: PieceGroups) = OpenSeries(group)
}
case object OpenPairs extends Action {
  def apply(pieces: List[Piece]*): OpenPairs = OpenPairs(pieces toList)

  override val key = "op"

  override def withPieceGroup(group: PieceGroups) = OpenPairs(group)
}
case object CollectOpen extends Action {
  override val key = "co"
}

case object LeaveTaken extends Action {
  override val key = "lt"
}

case object DropOpenSeries extends Action {
  override val key = "dos"

  override def withDrop(piece: Piece, pos: OpenPos) = DropOpenSeries(piece, pos)
}

case object DropOpenPairs extends Action {
  override val key = "dop"

  override def withDrop(piece: Piece, pos: OpenPos) = DropOpenPairs(piece, pos)
}

case class DropOpenSeries(piece: Piece, pos: OpenPos) extends Action {
  override def toSingle = DropOpenSeries

  override def toUci: Uci = Uci.Move(this, piece = Some(piece), openPos = Some(pos))
}

case class DropOpenPairs(piece: Piece, pos: OpenPos) extends Action {
  override def toSingle = DropOpenPairs

  override def toUci: Uci = Uci.Move(this, Some(piece), openPos = Some(pos))
}

case class DrawMiddle(piece: Piece) extends Action {
  override def toSingle = DrawMiddle

  override def toUci: Uci = Uci.Move(this, piece = Some(piece))
}

case class DrawLeft(piece: Piece) extends Action {
  override def toSingle = DrawLeft

  override def toUci: Uci = Uci.Move(this, piece = Some(piece))
}

case class Discard(piece: Piece) extends Action {
  override def toSingle = Discard

  override def toUci: Uci = Uci.Move(this, piece = Some(piece))
}

case class DiscardEndSeries(pieces: PieceGroups) extends Action {
  override def toSingle = DiscardEndSeries

  override def toUci: Uci = Uci.Move(this, group = Some(pieces))
}

case class DiscardEndPairs(pieces: PieceGroups) extends Action {
  override def toSingle = DiscardEndPairs

  override def toUci: Uci = Uci.Move(this, group = Some(pieces))
}

case class ShowSign(piece: Piece) extends Action {
  override def toSingle = ShowSign

  override def toUci: Uci = Uci.Move(this, piece = Some(piece))
}

case class OpenSeries(pieces: PieceGroups) extends Action {
  override def toSingle = OpenSeries

  override def toUci: Uci = Uci.Move(this, group = Some(pieces))
}
case class OpenPairs(pieces: PieceGroups) extends Action {
  override def toSingle = OpenPairs

  override def toUci: Uci = Uci.Move(this, group = Some(pieces))
}

case class CollectOpen(save: (Board, Opener)) extends Action {
  override def toSingle = CollectOpen
}


object Action {
  lazy val all: List[Action] = List(DrawLeft, DrawMiddle, Discard, DiscardEndSeries, DiscardEndPairs, ShowSign, OpenSeries, OpenPairs, CollectOpen, LeaveTaken, DropOpenSeries, DropOpenPairs)

  def byKey(key: String) = allKeys get key

  val allKeys: Map[String, Action] = all map { action => action.key -> action } toMap
}
