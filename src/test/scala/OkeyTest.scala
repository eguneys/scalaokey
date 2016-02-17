package okey

import org.specs2.mutable._
import ornicar.scalalib.test.ValidationMatchers

trait OkeyTest
    extends Specification
    with ValidationMatchers {

  def makeBoard(pieces: (Piece, Int)*): Board =
    Board(pieces)

  def makeBoard: Board = Board empty
}
