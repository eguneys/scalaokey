package okey

case class Opener(
  series: List[OpenSerie],
  pairs: List[OpenPair],
  opens: Sides[Option[OpenState]]) {

  import implicitFailures._

  def score(side: Side): Option[Int] = opens(side) map(_.score)

  def seqOpener(actions: Opener => Valid[Opener]*): Valid[Opener] =
    actions.foldLeft(success(this): Valid[Opener])(_ flatMap _)


  private def findScore(pieces: List[Piece]): Int =
    pieces.foldLeft(0) { _ + _.number }

  def openSeries(side: Side, pieces: PieceGroups, board: Option[Board] = None): Valid[Opener] = {
    val openState = opens(side) match {
      case None => board map (b => OpenState.series(findScore(pieces flatten), b, this))
      case Some(o: NewOpen) => Some(o.addScore(findScore(pieces flatten)))
      case Some(o: OldOpen) => Some(o)
    }

    openState.fold(
      failure("Cannot open new with no board"): Valid[Opener]) { os =>
      success(copy(
        series = series ::: pieces.map { ps => OpenSerie(side, ps) },
        opens = opens.withSide(side, Some(os))
      ))
    }
  }

  def openPairs(side: Side, pieces: PieceGroups, board: Option[Board] = None): Valid[Opener] = {
    val openState = opens(side) match {
      case None => board map (b => OpenState.pairs(pieces.length, b, this))
      case Some(o: NewOpen) => Some(o.addScore(pieces.length))
      case Some(o: OldOpen) => Some(o)
    }

    openState.fold(
      failure("Cannot open new with no board"): Valid[Opener]) { os =>
      success(copy(
        pairs = pairs ::: pieces.map { ps => OpenPair(side, ps) },
        opens = opens.withSide(side, Some(os))
      ))
    }
  }

  def commitOpen(side: Side): Valid[Opener] = opens(side) match {
    case Some(a: NewOpen) => success(copy(opens = opens.withSide(side, Some(a.commit))))
    case _ => failure("Cannot commit a non new open")
  }
}

object Opener {
  def empty = Opener(
    series = Nil,
    pairs = Nil,
    opens = Sides[Option[OpenState]])
}
