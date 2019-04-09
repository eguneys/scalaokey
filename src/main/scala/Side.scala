package okey

import ornicar.scalalib.Zero

sealed trait Side {
  val next: Side
  val previous: Side

  val name: String
  val letter: Char
}

object Side {

  case object EastSide extends Side {
    val next = NorthSide
    val previous = SouthSide
    val name = "east"
    val letter = 'e'
  }
  case object WestSide extends Side {
    val next = SouthSide
    val previous = NorthSide
    val name = "west"
    val letter = 'w'

  }
  case object NorthSide extends Side {
    val next = WestSide
    val previous = EastSide
    val name = "north"
    val letter = 'n'

  }
  case object SouthSide extends Side {
    val next = EastSide
    val previous = WestSide
    val name = "south"
    val letter = 's'
  }


  def apply(n: Int): Side = n % 4 match {
    case 0 => EastSide
    case 1 => NorthSide
    case 2 => WestSide
    case 3 => SouthSide
  }

  def apply(c: String): Option[Side] = c match {
    case "e" | "east" => EastSide.some
    case "w" | "west" => WestSide.some
    case "n" | "north" => NorthSide.some
    case "s" | "south" => SouthSide.some
    case _ => None
  }

  def apply(s: Char): Option[Side] = apply(s.toString)

  lazy val all: List[Side] = List(EastSide, WestSide, NorthSide, SouthSide)
}

case class Sides[A](
  eastSide: A,
  westSide: A,
  northSide: A,
  southSide: A) extends Iterable[A] {

  def iterator = List(eastSide, westSide, northSide, southSide).iterator

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

  // lazy val toList: List[A] = List(eastSide, westSide, northSide, southSide)

  // def foldLeft[B](b: B)(op: (B, A) => B): B =
  //   op(op(op(op(b, eastSide), westSide), northSide), southSide)

  // def fold[B: Semigroup](op: A => B): B =
  //   op(eastSide) |+| op(westSide) |+| op(northSide) |+| op(southSide)

  // def map[B](op: A => B): Sides[B] =
  //   Sides(op(eastSide), op(westSide), op(northSide), op(southSide))

  def sideMap[B](op: (Side, A) => B): Sides[B] =
    Sides(op(EastSide, eastSide),
      op(WestSide, westSide),
      op(NorthSide, northSide),
      op(SouthSide, southSide))

  def mapt[B](op: A => B): Sides[B] = this.map(op): Sides[B]
}

object Sides {

  def apply[A : Zero]: Sides[A] = Sides(
    zero[A],
    zero[A],
    zero[A],
    zero[A]
  )

  def apply[A](f: Side => A): Sides[A] = Sides(
    eastSide = f(EastSide),
    westSide = f(WestSide),
    northSide = f(NorthSide),
    southSide = f(SouthSide)
  )


  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext

  implicit def richSides[A](sides: Sides[Future[A]]) = new {
    def sequenceSides(implicit exec: ExecutionContext): Future[Sides[A]] = for {
      east <- sides.eastSide
      west <- sides.westSide
      north <- sides.northSide
      south <- sides.southSide
    } yield Sides(east, west, north, south)
  }

  implicit def richSidesOption[A](sides: Sides[Option[A]]) = new {
    def sequenceSides: Option[Sides[A]] = for {
      east <- sides.eastSide
      west <- sides.westSide
      north <- sides.northSide
      south <- sides.southSide
    } yield Sides(east, west, north, south)
  }

  implicit def fromIterable[A](l: Iterable[A]): Sides[A] = {
    val i = l.iterator
    Sides(
      i.next,
      i.next,
      i.next,
      i.next
    )
  }
}
