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
  case object MiddleEnd extends Status(30)
  case object NormalEnd extends Status(40) // from this point game round is counted
  case object VariantEnd extends Status(70) // the variant has a special ending

  val all = List(Created, Started, Aborted, NormalEnd, MiddleEnd, VariantEnd)

  val byId = all map { v => (v.id, v) } toMap

  def apply(id: Int): Option[Status] = byId get id

}
