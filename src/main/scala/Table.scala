package okey

import variant.{ Variant, Dealer }

case class Table(
  boards: Sides[Board],
  discards: Sides[List[Piece]] = Sides[List[Piece]],
  middles: List[Piece],
  opens: Option[(OpenSeries, OpenPairs)] = Some((List.empty[OpenSerie], List.empty[OpenPair])),
  sign: Piece,
  player: Side = EastSide) {

  lazy val fakeOkey: Piece = sign.up
}

object Table {
  def init(variant: Variant, player: Side = EastSide): Table = {
    val dealer = Dealer(player)
    Table(
      boards = dealer.boards,
      middles = dealer.middles,
      sign = dealer.sign,
      player = player
    )
  }
}
