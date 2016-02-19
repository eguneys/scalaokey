package okey

import ornicar.scalalib

sealed trait Side {
  val previous: Side
}

case object EastSide extends Side {
  val previous = SouthSide
}
case object WestSide extends Side {
  val previous = NorthSide
}
case object NorthSide extends Side {
  val previous = EastSide
}
case object SouthSide extends Side {
  val previous = WestSide
}

case class Sides[A](
  eastSide: A,
  westSide: A,
  northSide: A,
  southSide: A) {

  import scalaz._
  import Scalaz._

  def apply(side: Side): A = side match {
    case EastSide => eastSide
    case WestSide => westSide
    case NorthSide => northSide
    case SouthSide => southSide
  }

  def withSide(side: Side, a: A) = side match {
    case EastSide => copy(eastSide = a)
    case WestSide => copy(westSide = a)
    case NorthSide => copy(northSide = a)
    case SouthSide => copy(southSide = a)
  }

  def fold[B: Semigroup](op: A => B): B =
    op(eastSide) |+| op(westSide) |+| op(northSide) |+| op(southSide)

  def map[B](op: A => B): Sides[B] =
    Sides(op(eastSide), op(westSide), op(northSide), op(southSide))
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
