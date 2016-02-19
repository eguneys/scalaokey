package okey

sealed trait Color {
  val letter: Char
  val name: String

  def -(number: Int): Piece = Piece(this, number)
}

object Color {

  case object Red extends Color {
    val letter = 'r'
    val name = "red"
  }
  case object Black extends Color {
    val letter = 'l'
    val name = "black"
  }
  case object Green extends Color {
    val letter = 'g'
    val name = "green"
  }
  case object Blue extends Color {
    val letter = 'b'
    val name = "blue"
  }
  case object Fake extends Color {
    val letter = 'f'
    val name = "fake"
  }

  def apply(c: Char): Option[Color] = c match {
    case 'r' => Some(Red)
    case 'l' => Some(Black)
    case 'g' => Some(Green)
    case 'b' => Some(Blue)
    case 'f' => Some(Fake)
    case _ => None
  }

  val all = List(Red, Black, Green, Blue)
}
