/** Copyright (c) 2013 Saddle Development Team
  *
  * Licensed under the Apache License, Version 2.0 (the "License"); you may not
  * use this file except in compliance with the License. You may obtain a copy
  * of the License at
  *
  * http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  * License for the specific language governing permissions and limitations
  * under the License.
  */
package org

import scala.language.implicitConversions
import org.saddle.index.{SliceAll, Slice, SliceFrom, SliceTo}

// some typeclass interfaces we'll alias
import org.saddle.scalar.ScalarTag
import scala.reflect.ClassTag
import cats.kernel.Order
import org.saddle.order._

/** ==Saddle==
  *
  * Saddle is a '''S'''cala '''D'''ata '''L'''ibrary.
  *
  * Saddle provides array-backed, indexed one- and two-dimensional data
  * structures.
  *
  * These data structures are specialized on JVM primitives. With them one can
  * often avoid the overhead of boxing and unboxing.
  *
  * Basic operations also aim to be robust to missing values (NA's)
  *
  * The building blocks are intended to be easily composed.
  *
  * The foundational building blocks are:
  *
  *   - [[org.saddle.Vec]]
  *   - [[org.saddle.Mat]]
  *   - [[org.saddle.Index]]
  *   - [[org.saddle.Series]]
  *   - [[org.saddle.Frame]]
  *
  * Inspiration for Saddle comes from many sources, including the R programming
  * language, the pandas data analysis library for Python, and the Scala
  * collections library.
  */
package object saddle {
  // ********************** Some type aliases, save our fingers in typing

  /** Shorthand for ordering typeclass
    */
  type ORD[C] = Order[C]

  /** Shorthand for numeric typeclass
    */
  type NUM[C] = org.saddle.Numeric[C]

  /** Shorthand for class manifest typeclass
    */
  type CLM[C] = ClassTag[C]

  /** Shorthand for scalar tag typeclass
    */
  type ST[C] = ScalarTag[C]

  // *********************
  implicit val doubleOrd = doubleIsNumeric
  implicit val floatOrd = floatIsNumeric
  implicit val intOrd = intIsNumeric
  implicit val longOrd = longIsNumeric

  // **********************

  /** Allow timing of an operation
    *
    * {{{
    *   clock { bigMat.T dot bigMat }
    * }}}
    */
  def clock[T](op: => T): (Double, T) = {
    val s = System.nanoTime
    val r = op
    val e = System.nanoTime
    ((e - s) / 1e9, r)
  }

  /** Syntactic sugar, allow '->' to generate an (inclusive) index slice
    *
    * {{{
    *   val v = Vec(1,2,3,4)
    *   val u = v(0 -> 2)
    * }}}
    */
  implicit def pair2Slice[T](p: (T, T)) = Slice(p._1, p._2)
  implicit def any2Slice[T](p: T) = Slice(p, p)

  /** Syntactic sugar, allow '* -> ' to generate an (inclusive) index slice,
    * open on left
    *
    * {{{
    *   val v = Vec(1,2,3,4)
    *   val u = v(* -> 2)
    * }}}
    */
  implicit def pair2SliceTo[T](p: (SliceAll, T)) = SliceTo(p._2)

  /** Syntactic sugar, allow ' -> *' to generate an (inclusive) index slice,
    * open on right
    *
    * {{{
    *   val v = Vec(1,2,3,4)
    *   val u = v(1 -> *)
    * }}}
    */
  implicit def pair2SliceFrom[T](p: (T, SliceAll)) = SliceFrom(p._1)

  /** Syntactic sugar, placeholder for 'slice-all'
    *
    * {{{
    *   val v = Vec(1,2,3, 4)
    *   val u = v(*)
    * }}}
    */
  def * = SliceAll()

  /** `na` provides syntactic sugar for constructing primitives recognized as
    * NA. A use case is be:
    *
    * {{{
    *   Vec[Int](1,2,na,4)
    * }}}
    *
    * `na` will implicitly convert to a primitive having the designated missing
    * value bit pattern. That pattern is as follows:
    *
    *   1. byte => Byte.MinValue
    *   1. char => Char.MinValue
    *   1. short => Short.Minvalue
    *   1. int => Int.MinValue
    *   1. long => Long.MinValue
    *   1. float => Float.NaN
    *   1. double => Double.NaN
    *
    * The NA bit pattern for integral types is `MinValue` because it induces a
    * symmetry on the remaining bound of values; e.g. the remaining `Byte` bound
    * is (-127, +127).
    *
    * Note since `Boolean`s can only take on two values, it has no `na`
    * primitive bit pattern.
    */
  object na {

    /** Generates a primitive missing value bit pattern.
      */
    def to[T](implicit fn: na.type => T): T = fn(this)

    implicit val naToByte: na.type => Byte = (_: na.type) =>
      scalar.ScalarTagByte.missing
    implicit val naToChar: na.type => Char = (_: na.type) =>
      scalar.ScalarTagChar.missing
    implicit val naToShort: na.type => Short = (_: na.type) =>
      scalar.ScalarTagShort.missing

    implicit val naToInt: na.type => Int = (_: na.type) =>
      scalar.ScalarTagInt.missing
    implicit val naToLong: na.type => Long = (_: na.type) =>
      scalar.ScalarTagLong.missing
    implicit val naToFloat: na.type => Float = (_: na.type) =>
      scalar.ScalarTagFloat.missing
    implicit val naToDouble: na.type => Double = (_: na.type) =>
      scalar.ScalarTagDouble.missing

    override def toString = "na"
  }

  // Augment Seq with a few conversion methods
  //

  /** Augments Seq with a toVec method that returns a new Vec instance.
    *
    * For example,
    *
    * {{{
    *   val s = IndexedSeq(1,2,3)
    *   val v = s.toVec
    * }}}
    *
    * @param s
    *   A value of type Seq[T]
    * @tparam T
    *   Type of elements of Vec
    */
  implicit class SeqToVec[T: ST](s: Seq[T]) {
    def toVec: Vec[T] = Vec(s: _*)
  }
  implicit class ArrToVec[@specialized(Boolean, Int, Long, Double) T: ST](
      s: Array[T]
  ) {
    def toVec: Vec[T] = Vec(s)
  }

  implicit class SeqToMat[T: ST](
      s: Seq[Vec[T]]
  ) {
    def toMat = Mat(s: _*)
  }

  /** Augments Seq with a toIndex method that returns a new Index instance.
    *
    * For example,
    *
    * {{{
    *   val i = IndexedSeq(1,2,3)
    *   val s = i.toIndex
    * }}}
    *
    * @param ix
    *   A value of type Seq[X]
    * @tparam X
    *   Type of index elements
    */
  implicit class SeqToIndex[X: ST: ORD](ix: Seq[X]) {
    def toIndex: Index[X] = Index(ix: _*)
  }

  /** Augments Seq with a toSeries method that returns a new Series instance.
    *
    * For example,
    *
    * {{{
    *   val p = IndexedSeq(1,2,3) zip IndexedSeq(4,5,6)
    *   val s = p.toSeries
    * }}}
    *
    * @param s
    *   A value of type Seq[(X, T)]
    * @tparam T
    *   Type of data elements of Series
    * @tparam X
    *   Type of index elements of Series
    */
  implicit class SeqToSeries[T: ST, X: ST: ORD](s: Seq[(X, T)]) {
    def toSeries: Series[X, T] = Series(s: _*)
  }

  /** Augments Seq with a toFrame method that returns a new Frame instance.
    *
    * For example,
    *
    * {{{
    *   val t = IndexedSeq(("a", "x", 3), ("b", "y", 4))
    *   val f = t.toFrame
    *
    *   res0: org.saddle.Frame[java.lang.String,java.lang.String,Int] =
    *   [2 x 2]
    *         x  y
    *         -- --
    *   a ->  3 NA
    *   b -> NA  4
    * }}}
    *
    * @param s
    *   A value of type Seq[(RX, CX, T)]
    * @tparam T
    *   Type of data elements of Frame
    * @tparam RX
    *   Type of row index elements of Frame
    * @tparam CX
    *   Type of col index elements of Frame
    */
  implicit class SeqToFrame[RX: ST: ORD, CX: ST: ORD, T: ST](
      s: Seq[(RX, CX, T)]
  ) {
    def toFrame = {
      val grp = s.map { case (r, c, v) => ((r, c), v) }
      grp.toSeries.pivot
    }
  }

  implicit class SeqToFrame2[RX: ST: ORD, CX: ST: ORD, T: ST](
      s: Seq[(CX, Series[RX, T])]
  ) {
    def toFrame: Frame[RX, CX, T] = Frame(s: _*)
  }

  implicit class PrimitiveToScalar[
      @specialized(Boolean, Int, Long, Float, Double) T
  ](p: T)(implicit st: ST[T]) {
    def toScalar = org.saddle.scalar.Scalar(p)
    def isNA = st.isMissing(p)
  }

  implicit class OptionToScalar[
      @specialized(Boolean, Int, Long, Float, Double) T
  ](p: Option[T])(implicit st: ST[T]) {
    def toScalar =
      p match {
        case None    => org.saddle.scalar.NA
        case Some(v) => org.saddle.scalar.Scalar(v)
      }
  }

  /** Constant used in string byte-level manipulation
    */
  val UTF8 = "UTF-8"

  /** Specialized methods for Vec[Double]
    *
    * Methods in this class do not filter out NAs, e.g. Vec(NA,1d).max2 == NA
    * rather than 1d
    */
  implicit class VecDoubleOps(self: Vec[Double]) {

    /* Max. Does not filter out NAs*/
    def max2 = {
      var s = Double.MinValue
      var i = 0
      val n = self.length
      while (i < n) {
        val v = self.raw(i)
        if (v > s) {
          s = v
        }
        i += 1
      }
      s
    }

    /* Min. Does not filter out NAs*/
    def min2 = {
      var s = Double.MaxValue
      var i = 0
      val n = self.length
      while (i < n) {
        val v = self.raw(i)
        if (v < s) {
          s = v
        }
        i += 1
      }
      s
    }

    /* Sum. Does not filter out NAs*/
    def sum2 = {
      var s = 0d
      var i = 0
      val n = self.length
      while (i < n) {
        s += self.raw(i)
        i += 1
      }
      s
    }

    /* One pass mean using Welford's algorithm. Does not filter out NAs */
    def mean2 = {
      val n = self.length
      var xm = 0d
      var i = 0
      while (i < n) {
        val x = self.raw(i)
        xm += (x - xm) / (i + 1)
        i += 1
      }
      xm
    }

    /* One pass sample variance using Welford's algorithm. Does not filter out NAs */
    def sampleVariance = {
      val n = self.length
      var m = 0d
      var xm = 0d
      var i = 0
      while (i < n) {
        val x = self.raw(i)
        val tmp = xm
        xm += (x - xm) / (i + 1)
        m += (x - tmp) * (x - xm)
        i += 1
      }
      m / (n - 1)
    }

    def sampleStandardDeviation = math.sqrt(sampleVariance)

    /* Subtracts the mean from each element. Does not filter out NAs */
    def demeaned = {
      val mean = this.mean2
      val n = self.length
      val ar2 = Array.ofDim[Double](n)
      var i = 0
      while (i < n) {
        ar2(i) = self.raw(i) - mean
        i += 1
      }
      Vec(ar2)
    }

    /** One pass Pearson correlation coefficient. Does not filter out NAs.
      * https://prod-ng.sandia.gov/techlib-noauth/access-control.cgi/2008/086212.pdf
      */
    def pearson(other: Vec[Double]) = {
      val n = self.length
      assert(n == other.length)
      var covS = 0d
      var varS1 = 0d
      var varS2 = 0d
      var xm1 = 0d
      var xm2 = 0d
      var i = 0
      while (i < n) {
        val x1 = self.raw(i)
        val x2 = other.raw(i)
        val tmp1 = xm1
        val tmp2 = xm2
        xm1 += (x1 - xm1) / (i + 1)
        xm2 += (x2 - xm2) / (i + 1)
        covS += (x1 - tmp1) * (x2 - tmp2) * i / (i + 1)
        varS1 += (x1 - tmp1) * (x1 - xm1)
        varS2 += (x2 - tmp2) * (x2 - xm2)
        i += 1
      }
      covS / math.sqrt(varS1 * varS2)
    }

    /** One pass covariance of two vectors
      *
      * https://prod-ng.sandia.gov/techlib-noauth/access-control.cgi/2008/086212.pdf
      */
    def sampleCovariance(other: Vec[Double]) = {
      val n = self.length
      assert(n == other.length)
      var covS = 0d
      var xm1 = 0d
      var xm2 = 0d
      var i = 0
      while (i < n) {
        val x1 = self.raw(i)
        val x2 = other.raw(i)
        val tmp1 = xm1
        val tmp2 = xm2
        xm1 += (x1 - xm1) / (i + 1)
        xm2 += (x2 - xm2) / (i + 1)
        covS += (x1 - tmp1) * (x2 - tmp2) * i / (i + 1)
        i += 1
      }
      covS / (n - 1)
    }

  }

  /** Filling method for NA values. Non-sealed because could add more variants
    * in the future.
    */
  // private constructor to prevent public extensions
  abstract class FillMethod private[saddle] ()
  case object FillForward extends FillMethod() {}
  case object FillBackward extends FillMethod() {}
}
