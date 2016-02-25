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
}

case object DrawLeft extends Action
case object DrawMiddle extends Action
case object Discard extends Action
case object OpenSeries extends Action {
  def apply(pieces: List[Piece]*): OpenSeries = OpenSeries(pieces toList)
}
case object OpenPairs extends Action {
  def apply(pieces: List[Piece]*): OpenPairs = OpenPairs(pieces toList)
}
case object CollectOpen extends Action
case object LeaveTaken extends Action

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

object Action {
  lazy val all: List[Action] = List(DrawLeft, DrawMiddle, Discard, OpenSeries, OpenPairs, CollectOpen, LeaveTaken)
}
