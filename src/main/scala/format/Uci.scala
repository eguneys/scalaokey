package okey
package format

sealed trait Uci {
  def uci: String

  def action: Action
}

object Uci {

  case class Move(val action: Action) extends Uci {
    def uci = action.key
  }

  object Move {
    def apply(move: Action, piece: Option[Piece]): Move = Move(piece.fold(move) {
      move.withPiece(_)
    })

    def fromStrings(key: String, oPiece: Option[String] = None) = for {
      move <- (Action byKey key)
    } yield Move(move, oPiece.flatMap(Piece byKey))
  }
}
