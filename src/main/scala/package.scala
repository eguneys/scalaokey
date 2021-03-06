import ornicar.scalalib

package object okey

extends scalalib.Validation
    with scalalib.Common
    with scalalib.OrnicarOption
    with scalalib.Zeros

    with scalalib.Zero.Syntax

    with scalaz.std.OptionFunctions
    with scalaz.syntax.std.ToOptionOps
    with scalaz.syntax.std.ToOptionIdOps

    with scalaz.std.ListInstances
    with scalaz.std.StringInstances

    with scalaz.syntax.ToValidationOps
    with scalaz.syntax.ToFunctorOps

    with scalaz.syntax.std.ToBooleanOps
    with scalaz.syntax.ToIdOps {

  val Red = Color.Red
  val Black = Color.Black
  val Green = Color.Green
  val Blue = Color.Blue
  val Fake = Color.Fake

  val EastSide = Side.EastSide
  val WestSide = Side.WestSide
  val NorthSide = Side.NorthSide
  val SouthSide = Side.SouthSide

  type PieceMap = Map[Piece, Int]

  type PieceGroups = List[List[Piece]]

  object implicitFailures {
    implicit def stringToFailures(str: String): Failures = scalaz.NonEmptyList(str)
  }

  def parseIntOption(str: String): Option[Int] = try {
    Some(java.lang.Integer.parseInt(str))
  } catch {
    case e: NumberFormatException => None
  }
}
