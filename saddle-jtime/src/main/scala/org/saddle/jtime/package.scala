package org.saddle
import java.time.temporal.{TemporalAmount, Temporal}

package object jtime {
  implicit def stepperForTemporal[S <: TemporalAmount, T <: Temporal]
      : Stepper[S, T] = new Stepper[S, T] {}
}
