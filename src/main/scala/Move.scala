package okey

case class Move(
  player: Player,
  action: Action,
  before: Table,
  after: Table)

object Move {

}

sealed trait Action {
  def toSingle: Action = this

  def withPiece(piece: Piece): Action = this

  def withPieceGroup(group: PieceGroups): Action = this

  val key: String = toSingle.key
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

case class DrawMiddle(piece: Piece) extends Action {
  override def toSingle = DrawMiddle
}

case class DrawLeft(piece: Piece) extends Action {
  override def toSingle = DrawLeft
}

case class Discard(piece: Piece) extends Action {
  override def toSingle = Discard
}

case class OpenSeries(pieces: PieceGroups) extends Action {
  override def toSingle = OpenSeries
}
case class OpenPairs(pieces: PieceGroups) extends Action {
  override def toSingle = OpenPairs
}

case class CollectOpen(save: (Board, Opener)) extends Action {
  override def toSingle = CollectOpen
}


object Action {
  lazy val all: List[Action] = List(DrawLeft, DrawMiddle, Discard, OpenSeries, OpenPairs, CollectOpen, LeaveTaken)

  def byKey(key: String) = allKeys get key

  val allKeys: Map[String, Action] = all map { action => action.key -> action } toMap
}
