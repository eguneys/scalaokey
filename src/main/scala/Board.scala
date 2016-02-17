package okey

import Math.{ min, max }

case class Board(pieces: PieceMap) {
  def apply(piece: Piece): Int = pieces getOrElse(piece, 0)

  def apply(pieces: Piece*): Boolean = {
    val pieceMap = pieces.groupBy(identity).map(t => t._1 -> t._2.size)
    pieceMap == this.pieces
  }

  def place(piece: Piece): Option[Board] = apply(piece) match {
    case n => Some(copy(pieces = pieces + (piece -> (n + 1))))
  }

  def take(pieces: Piece*): Option[Board] = pieces.foldLeft(Some(this): Option[Board]) {
    case (Some(b), piece) =>
      b.pieces get piece map {
        case 1 => copy(pieces = b.pieces - piece)
        case n => copy(pieces = b.pieces + (piece -> (n - 1)))
      }
    case (None, _) => None
  }

  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Some(this): Option[Board])(_ flatMap _)
}

object Board {
  def apply(pieces: Traversable[(Piece, Int)]): Board =
    Board(pieces.toMap)

  def empty: Board = Board(Nil)
}
