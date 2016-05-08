package okey

sealed trait Clock {
  val limit: Int
  val side: Side
  val times: Sides[Float]
  val timerOption: Option[Double]

  def time(s: Side) = times(s)

  def outoftime(s: Side) = remainingTime(s) == 0

  def remainingTime(s: Side) = math.max(0, limit - elapsedTime(s))

  def elapsedTime(s: Side) = time(s)

  def addTime(s: Side, t: Float): Clock

  def estimateTotalTime = limit

  def emergTime: Int = math.round(math.min(20, math.max(2, estimateTotalTime / 15)))

  def isRunning = timerOption.isDefined

  def switch: Clock

  protected def now = System.currentTimeMillis / 1000d
}

case class RunningClock(
  limit: Int,
  side: Side,
  times: Sides[Float],
  timer: Double) extends Clock {

  val timerOption = Some(timer)

  override def elapsedTime(s: Side) = time(s) + {
    if (s == side) now - timer else 0
  }.toFloat

  def step = {
    val t = now
    val spentTime = (t - timer).toFloat
    addTime(
      side,
      math.max(0, spentTime)
    ).copy(
      side = side.next,
      timer = t)
  }

  def stepPly = {
    val t = now
    val spentTime = (t - timer).toFloat
    addTime(
      side,
      math.max(0, spentTime)
    ).copy(
      timer = t)
  }

  def addTime(s: Side, t: Float): RunningClock =
    copy(times = times.withSide(s, times(s) + t))

  def emptyTime(s: Side): RunningClock =
    copy(times = times.withSide(s, 0))

  def switch: RunningClock = copy(side = side.next)
}

case class PausedClock(
  limit: Int,
  side: Side,
  times: Sides[Float]) extends Clock {
  val timerOption = None

  def start = RunningClock(
    side = side,
    times = times,
    limit = limit,
    timer = now)

  def addTime(s: Side, t: Float): PausedClock =
    copy(times = times.withSide(s, times(s) + t))

  def switch: PausedClock = copy(side = side.next)
}



object Clock {

  def apply(limit: Int): PausedClock = PausedClock(
    limit = limit,
    side = EastSide,
    times = Sides(_ => 0f))

}
