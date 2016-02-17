package okey

sealed trait Color

object Color {

  case object Red extends Color
  case object Black extends Color
  case object Green extends Color
  case object Blue extends Color
  case object Fake extends Color


  val all = List(Red, Black, Green, Blue, Fake)
}
