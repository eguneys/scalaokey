import ornicar.scalalib

package object okey
    extends scalalib.Validation {

  val Red = Color.Red
  val Black = Color.Black
  val Green = Color.Green
  val Blue = Color.Blue

  type PieceMap = Map[Piece, Int]
}
