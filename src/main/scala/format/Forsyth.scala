package okey
package format

object Forsyth {



  def >>(game: Game, side: Side): String = {
    val table = game.table
    exportTable(table, side)
  }

  def exportTable(table: Table, side: Side): String = {
    List(
      table.boards(side).pieceList mkString,
      (table.discards map (_.mkString)).toList.mkString(" "),
      table.opener ?? (_.series map(_.pieces mkString) mkString(" ")),
      table.opener ?? (_.pairs map(_.pieces mkString) mkString(" ")),
      table.middles.size.toString + table.sign,
      side.letter
    )
  } mkString "/"

  def exportGroups(groups: PieceGroups): String =
    groups map(_.mkString) mkString(" ")
}
