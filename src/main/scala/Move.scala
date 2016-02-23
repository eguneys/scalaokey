package okey

case class Move(
  action: Action,
  before: Table,
  after: Table)

object Move {

}

sealed trait Action

case object DrawLeft extends Action
case object DrawMiddle extends Action
case object Discard extends Action
case object OpenSeries extends Action
case object OpenPairs extends Action
case object CollectOpen extends Action
case object LeaveTaken extends Action

case class Discard(piece: Piece) extends Action
case class OpenSeries(pieces: PieceGroups) extends Action
case class OpenPairs(pieces: PieceGroups) extends Action

object Action {
  lazy val all: List[Action] = List(DrawLeft, DrawMiddle, Discard, OpenSeries, OpenPairs, CollectOpen, LeaveTaken)
}
