import ornicar.scalalib

package object okey

    extends scalalib.Validation

    with scalalib.Zero.Syntax
    with scalalib.Zero.Instances

    with scalaz.syntax.std.ToOptionIdOps

    with scalaz.std.ListInstances
    with scalaz.std.StringInstances

    with scalaz.syntax.std.ToBooleanOps {

  val Red = Color.Red
  val Black = Color.Black
  val Green = Color.Green
  val Blue = Color.Blue
  val Fake = Color.Fake

  type PieceMap = Map[Piece, Int]

  type PieceGroups = List[List[Piece]]

  object implicitFailures {
    implicit def stringToFailures(str: String): Failures = scalaz.NonEmptyList(str)
  }
}
