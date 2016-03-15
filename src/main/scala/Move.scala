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

  val key: String
}

case object DrawLeft extends Action {
  val key = "dl"

  override def withPiece(piece: Piece) = DrawLeft(piece)
}

case object DrawMiddle extends Action {
  val key = "dm"
}

case object Discard extends Action {
  val key = "dd"

  override def withPiece(piece: Piece) = Discard(piece)
}

case object OpenSeries extends Action {
  def apply(pieces: List[Piece]*): OpenSeries = OpenSeries(pieces toList)

  val key = "os"
}
case object OpenPairs extends Action {
  def apply(pieces: List[Piece]*): OpenPairs = OpenPairs(pieces toList)

  val key = "op"
}
case object CollectOpen extends Action {
  val key = "co"
}

case object LeaveTaken extends Action {
  val key = "lt"
}

case class DrawLeft(piece: Piece) extends Action {
  override def toSingle = DrawLeft

  val key = toSingle.key
}

case class Discard(piece: Piece) extends Action {
  override def toSingle = Discard

  val key = toSingle.key
}

case class OpenSeries(pieces: PieceGroups) extends Action {
  override def toSingle = OpenSeries

  val key = toSingle.key
}
case class OpenPairs(pieces: PieceGroups) extends Action {
  override def toSingle = OpenPairs

  val key = toSingle.key
}

object Action {
  lazy val all: List[Action] = List(DrawLeft, DrawMiddle, Discard, OpenSeries, OpenPairs, CollectOpen, LeaveTaken)

  def byKey(key: String) = allKeys get key

  val allKeys: Map[String, Action] = all map { action => action.key -> action } toMap
}
