package okey
package format

sealed trait Uci {
  def uci: String

  def action: Action
}

object Uci
    extends scalaz.std.OptionInstances
    with scalaz.syntax.ToTraverseOps {

  case class Move(val singleAction: Action, piece: Option[Piece] = None, group: Option[PieceGroups] = None) extends Uci {

    def uci = action.key + pieceString + groupString

    def action = piece.fold(group.fold(singleAction) {
      singleAction.withPieceGroup(_)
    }) {
      singleAction.withPiece(_)
    }

    def pieceString = piece.fold("")("P" + _.toString)

    def groupString = group.fold("")("G" + _.map(_.mkString).mkString(" "))
  }

  object Move {

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

  def apply(move: String): Option[Uci] = {
    Action.byKey(move take 2) map { singleAction =>
      val rest = move drop 3
      move.lift(2) match {
        case Some('P') => Move(singleAction, piece = Piece byKey(rest))
        case Some('G') => Move(singleAction, group = Move.readGroups(rest))
        case _ => Move(singleAction)
      }
    }
  }

  def readList(moves: String): Option[List[Uci]] =
    moves.split('/').toList.map(apply).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString "/"
}
