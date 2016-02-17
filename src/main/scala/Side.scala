package okey

import ornicar.scalalib

sealed trait Side

case object EastSide extends Side
case object WestSide extends Side
case object NorthSide extends Side
case object SouthSide extends Side

case class Sides[A](
  eastSide: A,
  westSide: A,
  northSide: A,
  southSide: A) {

  def apply(side: Side): A = side match {
    case EastSide => eastSide
    case WestSide => westSide
    case NorthSide => northSide
    case SouthSide => southSide
  }

  def fold[B](op: A => List[B]): List[B] =
    op(eastSide) ::: op(westSide) ::: op(northSide) ::: op(southSide)
}

object Sides {
  import scalalib.Zero

  def apply[A : Zero]: Sides[A] = Sides(
    zero[A],
    zero[A],
    zero[A],
    zero[A]
  )
}
