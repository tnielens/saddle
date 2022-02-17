package org.saddle.index

/** Shifts `T`s by a multiple of a step size `S`.
  */
trait Stepper[S, T] {

  def negateStep(s: S): S = multiplyStep(s, -1)
  def multiplyStep(s: S, i: Int): S

  def plus(t: T, step: S): T
  def plus(t: T, step: S, n: Int): T = plus(t, multiplyStep(step, n))
  def minus(t: T, step: S): T = plus(t, negateStep(step))
  def minus(t: T, step: S, n: Int): T = plus(t, multiplyStep(step, -n))

  /** Number of steps separating `from` and `to`.
    *
    * If `to` does not align with the steps, return the step count until the
    * last step before `to`. Return -1 if the steps don't reach `to` and `step`
    * should be negated.
    */
  def stepsBetween(from: T, to: T, step: S): Int

  /** Number of steps separating exactly `from` and `to`.
    *
    * Return -1 `to` does not align with the steps or if the steps don't reach
    * `to` and `step` should be negated.
    */
  def exactStepsBetween(from: T, to: T, step: S): Int = {
    val steps = stepsBetween(from, to, step)
    if (steps >= 0 && plus(from, step, steps) == to) {
      steps
    } else {
      -1
    }
  }

  /** Whether the half-bounded range starting at `from` stepping by `step`
    * contains `to`.
    */
  def exactEnd(from: T, t: T, step: S): Boolean =
    exactStepsBetween(from, t, step) >= 0
}

object Stepper {
  implicit val stepperLong = new Stepper[Long, Long] {
    override def multiplyStep(s: Long, i: Int): Long = s * i
    override def plus(t: Long, step: Long): Long = t + step
    override def stepsBetween(from: Long, to: Long, step: Long): Int =
      if (from <= to) {
        ((to - from) / step).toInt
      } else {
        -1
      }
  }
  implicit val stepperInt = new Stepper[Int, Int] {
    override def multiplyStep(s: Int, i: Int): Int = s * i
    override def plus(t: Int, step: Int): Int = t + step
    override def stepsBetween(from: Int, to: Int, step: Int): Int =
      if (from <= to) {
        (to - from) / step
      } else {
        -1
      }
  }

  def apply[S, T](implicit s: Stepper[S, T]): Stepper[S, T] = s
}
