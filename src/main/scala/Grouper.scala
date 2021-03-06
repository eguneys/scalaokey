package okey

sealed abstract class Grouper(sign: Piece)
    extends scalaz.std.OptionInstances
    with scalaz.syntax.ToTraverseOps {

  lazy val okey: Piece = sign.up

  def findScore(pieces: List[Piece]): Int

  def series(pieces: List[Piece]): Option[OpenSerie] = OpenSerie(pieces, findScore(pieces)).some
  def pairs(pieces: List[Piece]): Option[OpenPair] = OpenPair(pieces, 1).some

  def dropPairs(pair: OpenPair, piece: Piece): Option[OpenPair] =
    replaceOkey(pair.pieces, piece) flatMap pairs

  def dropSeries(serie: OpenSerie, piece: Piece, at: OpenPos): Option[OpenSerie] = at match {
    case _:AppendLeft => series(piece :: serie.pieces)
    case _:AppendRight => series(serie.pieces :+ piece)
    case _:ReplaceOkey => replaceOkey(serie.pieces, piece) flatMap series
  }

  private def replaceOkey(pieces: List[Piece], piece: Piece): Option[List[Piece]] = pieces.indexOf(okey) match {
    case -1 => None
    case n => pieces.updated(n, piece).some
  }

  def seriesSeq(pieces: PieceGroups): Option[List[OpenSerie]] =
    pieces.map(group => series(group)).sequence

  def pairsSeq(pieces: PieceGroups): Option[List[OpenPair]] =
    pieces.map(group => pairs(group)).sequence
}

case class StandardGrouper(sign: Piece, withTore: Boolean = false) extends Grouper(sign) {

  case class PieceRef(piece: Piece, fake: Boolean = false) {
    def isOkey: Boolean = piece == okey && !fake
  }
  implicit def pieceRefToPiece(pieceRef: PieceRef): Piece = pieceRef.piece

  import Piece._

  def findScore(pieces: List[Piece]): Int = pieces.foldLeft(0) { _ + _.number }

  private def replaceFake(pieces: List[Piece]): List[PieceRef] = pieces map { case F1 => PieceRef(okey, true); case x => PieceRef(x) }

  def replaceOkey(pieces: List[PieceRef]): List[PieceRef] = {
    lazy val matchRainbow = {
      val ps = pieces.filterNot(_.isOkey)
      val r = (for {
        c <- (Color.all diff ps.map(_.color)).headOption
        n <- ps.headOption.map(_.number)
        if (ps.length < pieces.length)
      } yield PieceRef(Piece(c, n))) toList

      r ::: ps
    }

    lazy val matchSerie = pieces match {
      case o :: a :: b :: rest if (o.isOkey) => ((a, b) match {
        case (p1, p2) if p1.up == p2.piece => PieceRef(p1.down) :: p1 :: p2 :: rest
        case (p1, p2) if p1.down == p2.piece => PieceRef(p1.up) :: p1 :: p2 :: rest
        case _ => pieces
      })
      case a :: o :: b :: rest if (o.isOkey) => ((a, b) match {
        case (p1, p2) if p1.up.up == p2.piece => p1 :: PieceRef(p1.up) :: p2 :: rest
        case (p1, p2) if p1.down.down == p2.piece => p1 :: PieceRef(p1.down) :: p2 :: rest
        case _ => pieces
      })
      case a :: b :: o :: rest if (o.isOkey) => ((a, b) match {
        case (p1, p2) if p1.up == p2.piece => p1 :: p2 :: PieceRef(p2.up) :: rest
        case (p1, p2) if p1.down == p2.piece => p1 :: p2 :: PieceRef(p2.down) :: rest
        case _ => pieces
      })
      case a :: rest => a :: replaceOkey(rest)
      case Nil => Nil
    }

    lazy val matchPair = pieces match {
      case o :: a :: Nil if (o.isOkey) => a :: a :: Nil
      case a :: o :: Nil if (o.isOkey) => a :: a :: Nil
      case x => x
    }

    pieces match {
      case a :: b :: c :: rest if (Set(a, b, c).filterNot(_.isOkey).map(_.number).size == 1) => matchRainbow
      case a :: b :: c :: rest if (Set(a, b, c).filterNot(_.isOkey).map(_.color).size == 1) => matchSerie
      case _ => matchPair
    }
  }

  override def series(pieces: List[Piece]): Option[OpenSerie] = {
    val replacedFake = replaceFake(pieces)
    val replaced = replaceOkey(replacedFake) map (_.piece)
    isSeries(replaced) option OpenSerie(pieces, findScore(replaced))
  }

  override def pairs(pieces: List[Piece]): Option[OpenPair] = {
    val replacedFake = replaceFake(pieces)
    val replaced = replaceOkey(replacedFake) map (_.piece)
    isPairs(replaced) option OpenPair(pieces, 1)
  }


  private def isPairs(pieces: List[Piece]): Boolean = pieces match {
    case p1 :: p2 :: Nil if p1 == p2 => true
    case _ => false
  }

  private def isSeries(pieces: List[Piece]): Boolean =  colorSeries(pieces) || numberSeries(pieces)

  private def colorSeries(pieces: List[Piece]): Boolean = {
    pieces.headOption map { piece =>
      val all = allNumbers(piece.color)
      pieces.length >= 3 &&
      ((all containsSlice pieces) ||
      (all containsSlice pieces.reverse))
    } getOrElse false
  }

  private def numberSeries(pieces: List[Piece]): Boolean = {
    pieces.headOption map { piece =>
      val all = Piece.<>(piece.number)
      pieces.length >= 3 &&
      ((pieces diff all).isEmpty)
    } getOrElse false
  }

  private def torr(pieces: List[Piece]): List[Piece] =
    withTore.fold(pieces :+ pieces.head, pieces)


  lazy val allNumbers: Map[Color, List[Piece]] = Map(
    Red -> torr(Piece.allRed),
    Black -> torr(Piece.allBlack),
    Green -> torr(Piece.allGreen),
    Blue -> torr(Piece.allBlue)
  )
}
