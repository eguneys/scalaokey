package okey
package format

sealed trait Uci {
  def uci: String

  def action: Action
}

object Uci
    extends scalaz.std.OptionInstances
    with scalaz.syntax.ToTraverseOps {

  case class Move(val action: Action) extends Uci {
    def uci = action.key
  }

  object Move {
    def apply(move: Action, piece: Option[Piece], group: Option[PieceGroups]): Move = Move(piece.fold(group.fold(move) {
      move.withPieceGroup(_)
    }) {
      move.withPiece(_)
    })

    def fromStrings(key: String, pieceS: Option[String] = None, pieceGroupS: Option[String] = None) = for {
      move <- (Action byKey key)
      pieceGroup = pieceGroupS.flatMap(group => readGroups(group))
    } yield Move(move, pieceS.flatMap(Piece byKey), pieceGroup)

    val PieceR = """(r|l|g|b)([1-9][0-3]?)""".r

    def parsePieces(str: String): Option[List[Piece]] = {
      PieceR.findAllIn(str).toList map {
        case PieceR(c, n) => Piece(Color(c(0)) get, n.toInt).some
        case _ => None
      }
    }.sequence

    def readGroups(groups: String): Option[PieceGroups] =
      groups.split(' ').toList.map(parsePieces).sequence
  }
}
