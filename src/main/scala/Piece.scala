package okey

case class Piece(color: Color, number: Int) {

  lazy val up: Piece = copy(number = (number % 13) + 1)

  def -(number: Int) = this -> number
}

object Piece {

  val R1 = Piece(Red, 1)
  val R2 = Piece(Red, 2)
  val R3 = Piece(Red, 3)
  val R4 = Piece(Red, 4)
  val R5 = Piece(Red, 5)
  val R6 = Piece(Red, 6)
  val R7 = Piece(Red, 7)
  val R8 = Piece(Red, 8)
  val R9 = Piece(Red, 9)
  val R10 = Piece(Red, 10)
  val R11 = Piece(Red, 11)
  val R12 = Piece(Red, 12)
  val R13 = Piece(Red, 13)
  val L1 = Piece(Black, 1)
  val L2 = Piece(Black, 2)
  val L3 = Piece(Black, 3)
  val L4 = Piece(Black, 4)
  val L5 = Piece(Black, 5)
  val L6 = Piece(Black, 6)
  val L7 = Piece(Black, 7)
  val L8 = Piece(Black, 8)
  val L9 = Piece(Black, 9)
  val L10 = Piece(Black, 10)
  val L11 = Piece(Black, 11)
  val L12 = Piece(Black, 12)
  val L13 = Piece(Black, 13)
  val G1 = Piece(Green, 1)
  val G2 = Piece(Green, 2)
  val G3 = Piece(Green, 3)
  val G4 = Piece(Green, 4)
  val G5 = Piece(Green, 5)
  val G6 = Piece(Green, 6)
  val G7 = Piece(Green, 7)
  val G8 = Piece(Green, 8)
  val G9 = Piece(Green, 9)
  val G10 = Piece(Green, 10)
  val G11 = Piece(Green, 11)
  val G12 = Piece(Green, 12)
  val G13 = Piece(Green, 13)
  val B1 = Piece(Blue, 1)
  val B2 = Piece(Blue, 2)
  val B3 = Piece(Blue, 3)
  val B4 = Piece(Blue, 4)
  val B5 = Piece(Blue, 5)
  val B6 = Piece(Blue, 6)
  val B7 = Piece(Blue, 7)
  val B8 = Piece(Blue, 8)
  val B9 = Piece(Blue, 9)
  val B10 = Piece(Blue, 10)
  val B11 = Piece(Blue, 11)
  val B12 = Piece(Blue, 12)
  val B13 = Piece(Blue, 13)
  val F1 = Piece(Fake, 1)

  val all = List(R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11, L12, L13, G1, G2, G3, G4, G5, G6, G7, G8, G9, G10, G11, G12, G13, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, F1)

  val initial = all ::: all
}
