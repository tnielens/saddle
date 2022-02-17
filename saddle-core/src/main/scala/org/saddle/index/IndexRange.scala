package org.saddle.index

import org.saddle.index.Stepper
import org.saddle.locator.Locator
import org.saddle.scalar.ScalarTag
import org.saddle._
import scala.{specialized => spec, Array}

/** A lazy, strictly ascending or descending, evenly-spaced, end-exclusive
  * index.
  */
class IndexRange[
    @spec(Int, Long) S,
    @spec(Boolean, Int, Long, Double) T
] private[index] (
    val from: T,
    val step: S,
    val length: Int
)(implicit
    stepper: Stepper[S, T],
    stag: ST[T],
    ord1: ORD[T]
) extends Index[T] {

  1 until 5
  private lazy val toArr: Array[T] =
    Array.iterate(from, length)(x => stepper.plus(x, step))

  override protected def locator: Locator[T] = new Locator[T] {

    override def contains(key: T): Boolean = {
      val gap = stepper.exactStepsBetween(from, key, step)
      gap >= 0 && gap < length
    }

    override def get(key: T): Int = {
      val g = stepper.exactStepsBetween(from, key, step)
      if (g >= length) -1
      else if (g >= 0) g
      else -1
    }

    override def put(key: T, value: Int): Unit = sys.error("Not supported")

    override def count(key: T): Int = 1

    override def inc(key: T): Int = sys.error("Not supported")

    override def keys: Array[T] = toArr

    override def counts: Array[Int] = Array.fill(length)(1)

    override def size: Int = length

  }

  override def scalarTag: ScalarTag[T] = stag

  override def ord: ORD[T] = ord1

  override def toVec: Vec[T] = Vec(toArr)

  override def raw(loc: Int): T = stepper.plus(from, step, loc)

  override def take(locs: Array[Int]): Index[T] =
    Index(locs.map(l => stepper.plus(from, step, l)))

  override def without(locs: Array[Int]): Index[T] = ???

  // todo return an IndexRange if possible
  override def concat(other: Index[T]): Index[T] = Index(toArr ++ other.toArray)

  override def lsearch(t: T): Int = stepper.exactStepsBetween(from, t, step)

  override def rsearch(t: T): Int = stepper.exactStepsBetween(from, t, step)

  override def slice(from: Int, until: Int, stride: Int): Index[T] =
    new IndexRange(
      stepper.plus(IndexRange.this.from, step, from),
      stepper.multiplyStep(step, stride),
      until - from
    )

  override def reversed: Index[T] =
    new IndexRange(last.getOrElse(from), stepper.negateStep(step), length)

  // Intersects two indices if both have set semantics
  def intersect(other: Index[T]): ReIndexer[T] = {
    if (!this.isUnique || !other.isUnique)
      throw Index.IndexException("Cannot intersect non-unique indexes")
    JoinerImpl.join(this, other, InnerJoin)
  }

  // Unions two indices if both have set semantics
  def union(other: Index[T]): ReIndexer[T] = {
    if (!this.isUnique || !other.isUnique)
      throw Index.IndexException("Cannot union non-unique indexes")
    JoinerImpl.join(this, other, OuterJoin)
  }

  override def isMonotonic: Boolean = true

  override def isContiguous: Boolean = true

  override def argSort: Array[Int] =
    if (ord.lt(from, stepper.plus(from, step)))
      (0 to length).toArray
    else
      ((length - 1) until 0 by -1).toArray

  override def join(other: Index[T], how: JoinType): ReIndexer[T] =
    JoinerImpl.join(this, other, how)

  override def map[@spec(Boolean, Int, Long, Double) B: ST: ORD](
      f: T => B
  ): Index[B] = Index(toArr.map(f))

  override private[saddle] def toArray: Array[T] = toArr
}

object IndexRange {

  def endExclusive[@spec(Int, Long) S, @spec(Boolean, Int, Long, Double) T](
      from: T,
      to: T,
      step: S
  )(implicit
      stepper: Stepper[S, T],
      stag: ST[T],
      ord1: ORD[T]
  ): IndexRange[S, T] = {
    val steps = stepper.stepsBetween(from, to, step)
    if (steps >= 0) {
      val length = if (stepper.exactEnd(from, to, step)) steps - 1 else steps
      new IndexRange(from, step, length)
    } else {
      new IndexRange(from, step, 0)
    }
  }

  def apply[@spec(Int, Long) S, @spec(Int, Long, Boolean, Double) T](
      from: T,
      step: S,
      length: Int
  )(implicit
      stepper: Stepper[S, T],
      stag: ST[T],
      ord1: ORD[T]
  ): IndexRange[S, T] = new IndexRange(from, step, length)
}
