package okey

sealed trait Color {
  def -(number: Int) = Piece(this, number)
}

object Color {

  case object Red extends Color
  case object Black extends Color
  case object Green extends Color
  case object Blue extends Color
}
