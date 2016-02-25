package okey

case class Actor(player: Player, table: Table) {

  import Function.const
  import implicitFailures._

  def toMove(action: Action): Option[Move] = {
    if (!moves.contains(action.toSingle))
      None
    else
      (action match {
        case OpenSeries(pieces) =>
          move(action) { table.openSeries(side, pieces) }
        case OpenPairs(pieces) =>
          move(action) { table.openPairs(side, pieces) }
        case CollectOpen =>
          move(action) { table.collectOpen(side) }
        case Discard(piece) =>
          move(action) { table.discard(side, piece) }
        case _ => None
      })
  }

  lazy val moves: List[Action] = player.drawPiece fold(afterDraw, draw)

  def draw: List[Action] = List(drawMiddle, drawLeft) flatten

  def drawMiddle: Option[Action] = table.drawMiddle(side).toOption map const(DrawMiddle)
  def drawLeft: Option[Action] = table.drawLeft(side).toOption map const(DrawLeft)

  def afterDraw: List[Action] = player.discardPiece.isDefined.fold(
    Nil,
    player.drawMiddle.fold(
      afterDrawMiddle,
      afterDrawLeft
    )
  )

  def afterDrawMiddle: List[Action] = List(discard, openSeries, openPairs, collectOpen) flatten

  def afterDrawLeft: List[Action] = List(openSeries, openPairs, collectOpen, leaveTaken) flatten

  def discard: Option[Action] = table.opens(side) flatMap {
    case Some(NewOpen(SerieScore(score), _, _)) if (score > minValidOpenSeriesScore) => Discard.some
    case Some(NewOpen(PairScore(score), _, _)) if (score > minValidOpenPairsScore) => Discard.some
    case Some(_:OldOpen) => Discard.some
    case _ => None
  }

  def openSeries: Option[Action] = (!hasOpenedPairs).option(OpenSeries)
  def openPairs: Option[Action] = (!hasOpenedSeries).option(OpenPairs)

  def collectOpen: Option[Action] = CollectOpen.some

  def leaveTaken: Option[Action] = LeaveTaken.some

  lazy val minValidOpenSeriesScore: Int = table.opener flatMap (_.maxOpenSerieScore) getOrElse 100

  lazy val minValidOpenPairsScore: Int = table.opener flatMap (_.maxOpenPairScore) getOrElse 4

  lazy val side = player.side

  lazy val hasOpenedPairs = table.hasOpenedPairs(side)
  lazy val hasOpenedSeries = table.hasOpenedSeries(side)

  private def move(action: Action)(vafter: Valid[Table]): Option[Move] = vafter map(move(action, _)) toOption

  private def move(
    action: Action,
    after: Table) = Move(
      player = player,
      action = action,
      before = table,
      after = after)
}
