package org.saddle.util

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Prop._

class UtilCheck extends Specification with ScalaCheck {

  "uprounding division" in {
    forAll { (a: Int, b: Int) =>
      (a >= 0 && b > 0) ==> {
        org.saddle.util
          .dividePositiveRoundUp(a, b) == math.ceil(a / b.toDouble).toInt
      }
    }
  }

}
