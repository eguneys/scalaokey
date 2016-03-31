package okey

case class Actor(player: Player, table: Table) {

  import Function.const
  import implicitFailures._

  def toMove(action: Action): Option[Move] = {
    if (!moves.contains(action.toSingle))
      None
    else
      (action match {
        case DrawMiddle =>
          table.toDrawMiddle(side) flatMap { p =>
            move(DrawMiddle(p)) { table.drawMiddle(side) }
          }
        case DrawLeft => {
          table.toDrawLeft(side) flatMap { p =>
            move(DrawLeft(p)) { table.drawLeft(side) }
          }
        }
        case LeaveTaken =>
          move(action) {
            player.drawLeft toValid "Not drawn left" flatMap (table.leaveTaken(side, _))
          }
        case OpenSeries(pieces) =>
          Grouper.seriesSeq(pieces) flatMap { series =>
            move(action) { table.openSeries(side, series) }
          }
        case OpenPairs(pieces) =>
          Grouper.pairsSeq(pieces) flatMap { pairs =>
            move(action) { table.openPairs(side, pairs) }
          }
        case CollectOpen =>
          table.toCollectOpen(side) flatMap { save =>
            move(CollectOpen(save)) { table.collectOpen(side) }
          }
        case Discard(piece) =>
          move(action) { table.discard(side, piece) }
        case _ => None
      })
  }

  lazy val moves: List[Action] = player.drawPiece fold(afterDraw, draw)

  def draw: List[Action] = List(drawMiddle, drawLeft) flatten

  def drawMiddle: Option[Action] = table.drawMiddle(side).toOption map const(DrawMiddle)
  def drawLeft: Option[Action] = table.drawLeft(side).toOption map const(DrawLeft)

  def afterDraw: List[Action] = player.drawMiddle.fold(
    afterDrawMiddle,
    afterDrawLeft
  )

  def afterDrawMiddle: List[Action] = List(discardMiddle, openSeries, openPairs, collectOpen) flatten

  def afterDrawLeft: List[Action] = List(discardLeft, openSeries, openPairs, collectOpen, leaveTaken) flatten

  def discardLeft: Option[Action] = discard(true)
  def discardMiddle: Option[Action] = discard(false)

  def discard(withLeft: Boolean): Option[Action] = table.opens(side) match {
    case Some(NewOpen(SerieScore(score), _, _)) if (score > minValidOpenSeriesScore) => Discard.some
    case Some(NewOpen(PairScore(score), _, _)) if (score > minValidOpenPairsScore) => Discard.some
    case Some(_:OldOpen) => Discard.some
    case None => withLeft.fold(None, Discard.some)
    case _ => None
  }

  def openSeries: Option[Action] = (!hasOpenedPairs).option(OpenSeries)
  def openPairs: Option[Action] = (!hasOpenedSeries).option(OpenPairs)

  def collectOpen: Option[Action] = table.opens(side) match {
    case Some(_:NewOpen)=> CollectOpen.some
    case _ => None
  }

  def leaveTaken: Option[Action] = LeaveTaken.some

  lazy val minValidOpenSeriesScore: Int = table.opener flatMap (_.maxOpenSerieScore) getOrElse 100

  lazy val minValidOpenPairsScore: Int = table.opener flatMap (_.maxOpenPairScore) getOrElse 4

  lazy val side = player.side

  lazy val hasOpenedPairs = table.hasOpenedPairs(side)
  lazy val hasOpenedSeries = table.hasOpenedSeries(side)

  def is(s: Side) = s == player.side

  private def move(action: Action)(vafter: Valid[Table]): Option[Move] = vafter map(move(action, _)) toOption

  private def move(
    action: Action,
    after: Table) = Move(
      player = player,
      action = action,
      before = table,
      after = after)
}
