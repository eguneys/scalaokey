package okey

import variant.{ Variant, Dealer }

case class Table(
  boards: Sides[Board],
  discards: Sides[List[Piece]] = Sides[List[Piece]],
  middles: List[Piece],
  opener: Option[Opener],
  sign: Piece,
  variant: Variant) {

  import implicitFailures._

  lazy val okey: Piece = sign.up

  def actorOf(player: Player): Actor = Actor(player, this)

  def seqTable(actions: Table => Valid[Table]*): Valid[Table] =
    actions.foldLeft(success(this): Valid[Table])(_ flatMap _)

  def drawMiddle(side: Side): Valid[Table] = (for {
    p <- middles.headOption
    b1 <- boards(side) place(p)
  } yield {
    copy(middles = middles.tail, boards = boards.withSide(side, b1))
  }) toValid "Cannot draw from empty middle"

  def drawLeft(side: Side): Valid[Table] = {
    val dside = side.previous
      (for {
        discard <- discards(dside).headOption
        d1 = discards(dside).tail
        b1 <- boards(side) place discard
      } yield {
        copy(boards = boards.withSide(side, b1),
          discards = discards.withSide(dside, d1))
      }) toValid "No piece on discards " + dside
  }

  def discard(side: Side, piece: Piece): Valid[Table] = (for {
    b1 <- boards(side) take piece
    d1 = piece :: discards(side)
  } yield { copy(boards = boards.withSide(side, b1),
    discards = discards.withSide(side, d1))
  }) toValid "No piece on board " + piece


  def openSeries(side: Side, pieces: PieceGroups): Valid[Table] = (for {
    b1 <- boards(side) take (pieces flatten)
    o1 <- opener
    o2 <- o1.openSeries(side, pieces, boards(side))
  } yield {
    copy(boards = boards.withSide(side, b1), opener = Some(o2))
  }) toValid "No piece on board " + pieces


  def openPairs(side: Side, pieces: List[List[Piece]]): Valid[Table] = (for {
    b1 <- boards(side) take (pieces flatten)
    o1 <- opener
    o2 <- o1.openPairs(side, pieces, boards(side))
  } yield {
    copy(boards = boards.withSide(side, b1), opener = Some(o2))
  }) toValid "No piece on board " + pieces

  def collectOpen(side: Side): Valid[Table] = (for {
    o1 <- opener
    o2 <- o1.collectOpen(side)
    b1 <- o1.boardSave(side)
  } yield {
    copy(boards = boards.withSide(side, b1), opener = Some(o2))
  }) toValid "No opener on table"

  def leaveTaken(side: Side, piece: Piece): Valid[Table] = {
    val dside = side.previous
    (for {
      b1 <- boards(side) take piece
      d1 = piece :: discards(dside)
    } yield {
      copy(boards = boards.withSide(side, b1),
        discards = discards.withSide(dside, d1))
    }) toValid "No piece on board " + piece
  }

  def toDrawMiddle(side: Side): Option[Piece] = middles.headOption

  def toDrawLeft(side: Side): Option[Piece] = discards(side previous).headOption

  def toCollectOpen(side: Side): Option[(Board, Opener)] = opener.flatMap (_.getSave)

  def opens(side: Side): Option[OpenState] = opener flatMap (_.opens(side))

  // def hasOpenedSeries(side: Side): Boolean =  !(opener ?? { _.seriesOf(side) isEmpty })

  // def hasOpenedPairs(side: Side): Boolean =  !(opener ?? { _.pairsOf(side) isEmpty })

  def hasOpenedSeries(side: Side): Boolean = opener ?? {
    _.score(side) match {
      case Some(_:SerieScore) => true
      case _ => false
    }
  }

  def hasOpenedPairs(side: Side): Boolean = opener ?? {
    _.score(side) match {
      case Some(_:PairScore) => true
      case _ => false
    }
  }


  def visual = format.Visual >> this

  override def toString = List(
    visual
  ) mkString "\n"
}

object Table {
  def init(variant: Variant, player: Side = EastSide): Table = {
    val dealer = Dealer(player)
    Table(
      boards = dealer.boards,
      middles = dealer.middles,
      sign = dealer.sign,
      opener = Some(Opener empty),
      variant = variant
    )
  }
}
