package okey

import ornicar.scalalib

sealed trait Side {
  val next: Side
  val previous: Side
  val letter: Char
}

case object EastSide extends Side {
  val next = NorthSide
  val previous = SouthSide
  val letter = 'e'
}
case object WestSide extends Side {
  val next = SouthSide
  val previous = NorthSide
  val letter = 'w'

}
case object NorthSide extends Side {
  val next = WestSide
  val previous = EastSide
  val letter = 'n'

}
case object SouthSide extends Side {
  val next = EastSide
  val previous = WestSide
  val letter = 's'
}

object Side {
  def apply(c: Char): Side = c match {
    case 'e' => EastSide
    case 'w' => WestSide
    case 'n' => NorthSide
    case 's' => SouthSide
  }

  lazy val all: List[Side] = List(EastSide, WestSide, NorthSide, SouthSide)
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

  def foldLeft[B](b: B)(op: (B, A) => B): B =
    op(op(op(op(b, eastSide), westSide), northSide), southSide)

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
