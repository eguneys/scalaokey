package okey
package format

sealed trait Uci {
  def uci: String

  def action: Action
}

object Uci
    extends scalaz.std.OptionInstances
    with scalaz.syntax.ToTraverseOps {

  case class Move(
    val singleAction: Action,
    piece: Option[Piece] = None,
    group: Option[PieceGroups] = None,
    openPos: Option[OpenPos] = None) extends Uci {

    def uci = action.key + pieceString + groupString + posString

    def action = piece.fold(group.fold(singleAction) {
      singleAction.withPieceGroup(_)
    }) { piece =>
      openPos.fold(singleAction.withPiece(piece)) { pos =>
        singleAction.withDrop(piece, pos)
      }
    }

    def pieceString = piece.fold("")("P" + _.toString)

    def groupString = group.fold("")("G" + _.map(_.mkString).mkString(" "))

    def posString = openPos.fold("")("@" + _.toString)
  }

  object Move {

    def fromStrings(
      key: String,
      pieceS: Option[String] = None,
      pieceGroupS: Option[String] = None,
      posS: Option[String] = None) = for {
      move <- (Action byKey key)
      pieceGroup = pieceGroupS.flatMap(group => readGroups(group))
      openPos = posS flatMap OpenPos.apply
    } yield Move(move, pieceS.flatMap(Piece byKey), pieceGroup, openPos)

    val PieceR = """(r|l|g|b)([1-9][0-3]?)""".r
    val PieceFakeR = """(r|l|g|b|f)([1-9][0-3]?)""".r

    def parsePieces(str: String): Option[List[Piece]] = {
      PieceFakeR.findAllIn(str).toList map {
        case PieceFakeR("f", n) => Piece.F1.some
        case PieceFakeR(c, n) => Piece(Color(c(0)) get, n.toInt).some
        case _ => None
      }
    }.sequence

    def readGroups(groups: String): Option[PieceGroups] =
      groups.split(' ').toList.map(parsePieces).sequence
  }

  def apply(move: String): Option[Uci] = applyDrop(move) orElse applyMove(move)

  val moveR = """^([a-z]{2}[a-z]?)(?:(P|G)(.*))?""".r
  def applyMove(move: String): Option[Uci] = move match {
    case moveR(actionS, pOrG, rest) => Action.byKey(actionS) map { singleAction =>
      pOrG match {
        case "P" => Move(singleAction, piece = Piece byKey(rest))
        case "G" => Move(singleAction, group = Move.readGroups(rest))
        case _ => Move(singleAction)
      }
    }

  }
    // Action.byKey(move take 2) map { singleAction =>
    //   val rest = move drop 3
    //   move.lift(2) match {
    //     case Some('P') => Move(singleAction, piece = Piece byKey(rest))
    //     case Some('G') => Move(singleAction, group = Move.readGroups(rest))
    //     case _ => Move(singleAction)
    //   }
    // }

  val dropR = """^(dop|dos)P([^@]*)@(.*)""".r
  def applyDrop(move: String): Option[Uci] = move match {
    case dropR(actionS, pieceS, posS) => Action.byKey(actionS) map { singleAction =>
      val piece = Piece.byKey(pieceS)
      val pos = OpenPos(posS)
      Move(singleAction, piece = piece, openPos = pos)
    }
    case _ => None
  }

  def readList(moves: String): Option[List[Uci]] =
    moves.split('/').toList.map(apply).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString "/"
}
