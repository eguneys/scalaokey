package okey

sealed abstract class Status(val id: Int) extends Ordered[Status] {
  val name = toString.head.toLower + toString.tail

  def compare(other: Status) = id compare other.id

  def is(s: Status): Boolean = this == s
}

object Status {
  case object Created extends Status(10)
  case object Started extends Status(20)
  case object Aborted extends Status(25) // from this point game is finished
  case object End extends Status(30)

  val all = List(Created, Started, Aborted, End)

  val byId = all map { v => (v.id, v) } toMap

  def apply(id: Int): Option[Status] = byId get id

}
