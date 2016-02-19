package okey

import variant.{ Variant, Dealer }

case class Table(
  boards: Sides[Board],
  discards: Sides[List[Piece]] = Sides[List[Piece]],
  middles: List[Piece],
  opens: Option[(OpenSeries, OpenPairs)] = Some((List.empty[OpenSerie], List.empty[OpenPair])),
  sign: Piece) {

  import implicitFailures._

  lazy val fakeOkey: Piece = sign.up

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


  def openSequence(side: Side, pieces: List[List[Piece]]): Valid[Table] = (for {
    b1 <- boards(side) take (pieces flatten)
    (series, pairs) <- opens
    os = pieces map OpenSerie.apply
    s1 = series ::: os
  } yield {
    copy(boards = boards.withSide(side, b1),
      opens = Some((s1, pairs)))
  }) toValid "No piece on board " + pieces


  def openPairs(side: Side, pieces: List[List[Piece]]): Valid[Table] = (for {
    b1 <- boards(side) take (pieces flatten)
    (series, pairs) <- opens
    opairs = pieces map OpenPair.apply
    p1 = pairs ::: opairs
  } yield {
    copy(boards = boards.withSide(side, b1),
      opens = Some((series, p1)))
  }) toValid "No piece on board " + pieces

}

object Table {
  def init(variant: Variant, player: Side = EastSide): Table = {
    val dealer = Dealer(player)
    Table(
      boards = dealer.boards,
      middles = dealer.middles,
      sign = dealer.sign
    )
  }
}
