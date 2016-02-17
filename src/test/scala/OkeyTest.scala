package okey

import org.specs2.mutable._
import ornicar.scalalib.test.ValidationMatchers

trait OkeyTest
    extends Specification
    with ValidationMatchers {

  def makeTable: Table = Table init okey.variant.Standard

  def makeTable(side: Side): Table =
    Table init(okey.variant.Standard, side)

  def makeBoard(pieces: Piece*): Board =
    Board(pieces)

  def makeBoard: Board = Board empty
}
