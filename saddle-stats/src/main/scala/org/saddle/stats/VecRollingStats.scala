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
package org.saddle.stats

import scala.{specialized => spec}
import org.saddle._
import ops._
import org.saddle.scalar._

/** Rolling statistical methods made available on numeric Vec objects via
  * enrichment. These methods scan over the Vec and compute values over a
  * specified historical window.
  */
class VecRollingStats[
    @spec(Int, Long, Double) A: ST: AddOp: SubOp: NUM
](v: Vec[A]) {

  /** Rolling count; compute count of number of elements in Vec over a sliding
    * window, ignoring any NA values.
    * @param winSz
    *   Size of the sliding window
    */
  def rollingCount(winSz: Int): Vec[Int] = v.rolling(winSz, new RollingCount[A])

  /** Rolling sum; compute sum of elements in Vec over a sliding window,
    * ignoring any NA values.
    * @param winSz
    *   Size of the sliding window
    */
  def rollingSum(winSz: Int): Vec[A] = v.rolling(winSz, new RollingSum[A])

  /** Rolling mean; compute mean of elements in Vec over a sliding window,
    * ignoring any NA values.
    * @param winSz
    *   Size of the sliding window
    */
  def rollingMean(winSz: Int): Vec[Double] =
    v.rolling(winSz, new RollingMean[A])

  /** Rolling median; compute median of elements in Vec over a sliding window,
    * ignoring any NA values.
    * @param winSz
    *   Size of the sliding window
    */
  def rollingMedian(winSz: Int): Vec[Double] =
    new RollingMedian[A](winSz, v).evaluate
}

class RollingCount[@spec(Int, Long, Double) A: ST: NUM]
    extends Function1[Vec[A], Int] {
  var i = 0
  var s = 0
  val sa = implicitly[ST[A]]
  var p = Scalar(sa.zero)

  def apply(v: Vec[A]): Int = {
    if (i == 0) {
      s = v.count
      i += 1
      if (v.length > 0) p = v.first
    } else {
      if (!p.isNA) s -= 1
      if (!v.last.isNA) s += 1
      p = v.first
    }
    s
  }
}

class RollingSum[@spec(Int, Long, Double) A: ST: AddOp: SubOp: NUM]
    extends Function1[Vec[A], A] {
  var i = 0
  val sa = implicitly[ST[A]]
  val add = implicitly[AddOp[A]]
  val sub = implicitly[SubOp[A]]
  var s = sa.zero
  var p = Scalar(sa.zero)

  def apply(v: Vec[A]): A = {
    if (i == 0) {
      s = v.sum
      i += 1
      if (v.length > 0) p = v.first
    } else {
      if (!p.isNA) s = sub(s, p.get)
      if (!v.last.isNA) s = add(s, v.last.get)
      p = v.first
    }
    s
  }
}

class RollingMean[@spec(Int, Long, Double) A: ST: NUM]
    extends Function1[Vec[A], Double] {
  var i = 0
  var s = 0d
  var c = 0
  val sa = implicitly[ST[A]]
  var p = Scalar(sa.zero)

  def apply(v: Vec[A]): Double = {
    if (i == 0) {
      s = sa.toDouble(v.sum)
      c = v.count
      i += 1
      if (v.length > 0) p = v.first
    } else {
      if (!p.isNA) {
        s -= sa.toDouble(p.get)
        c -= 1
      }
      if (!v.last.isNA) {
        s += sa.toDouble(v.last.get)
        c += 1
      }
      p = v.first
    }
    s / c
  }
}

class RollingMedian[@spec(Int, Long, Double) A: ST: NUM](
    winSz: Int,
    origv: Vec[A]
) {
  val sa = implicitly[ST[A]]

  val len = origv.length
  val win = if (winSz > len) len else winSz

  def evaluate: Vec[Double] = {
    if (len == 0 || winSz <= 0)
      Vec.empty
    else {
      val m = new Mediator(win)
      val r = Array.ofDim[Double](len - win + 1)

      var i = 0
      while (i < win) {
        val v = sa.toDouble(origv.raw(i))
        m.push(v)
        i += 1
      }

      r(0) = m.median

      var j = 1
      while (i < len) {
        val v = sa.toDouble(origv.raw(i))
        m.push(v)
        i += 1
        r(j) = m.median
        j += 1
      }

      Vec(r)
    }
  }
}
