package okey

import Math.{ min, max }

case class Board(pieces: PieceMap) {
  def apply(piece: Piece): Int = pieces getOrElse(piece, 0)

  def place(piece: Piece): Option[Board] = apply(piece) match {
    case n => Some(copy(pieces = pieces + (piece -> (n + 1))))
  }

  def place(pieces: List[Piece]): Option[Board] = pieces.foldLeft(Some(this): Option[Board]) {
    case (Some(b), piece) => b place piece
    case (None, _) => None
  }

  def take(pieces: Piece*): Option[Board] = take(pieces toList)

  def take(pieces: List[Piece]): Option[Board] = pieces.foldLeft(Some(this): Option[Board]) {
    case (Some(b), piece) =>
      b.pieces get piece map {
        case 1 => copy(pieces = b.pieces - piece)
        case n => copy(pieces = b.pieces + (piece -> (n - 1)))
      }
    case (None, _) => None
  }

  def seq(actions: Board => Option[Board]*): Option[Board] =
    actions.foldLeft(Some(this): Option[Board])(_ flatMap _)

  lazy val size: Int = pieces.values.fold(0)(_+_)

  lazy val pieceList: List[Piece] = pieces.foldLeft(List.empty[Piece]: List[Piece]) {
    case (acc, (p, count)) => List.fill(count)(p) ::: acc
  }
}

object Board {
  def apply(pieces: Traversable[Piece]): Board =
    Board(pieces groupBy identity map (t => t._1 -> t._2.size) toMap)

  def empty: Board = Board(List.empty[Piece])
}
