package org.saddle.util

import org.saddle.ORD
import cats.kernel.Order

trait OrderInstances {
  implicit def intOrd: ORD[Int] =
    cats.kernel.instances.int.catsKernelStdOrderForInt
  implicit def longOrd: ORD[Long] =
    cats.kernel.instances.long.catsKernelStdOrderForLong
  implicit def doubleOrd: ORD[Double] =
    cats.kernel.instances.double.catsKernelStdOrderForDouble
  implicit def charOrd: ORD[Char] =
    cats.kernel.instances.char.catsKernelStdOrderForChar
  implicit def floatOrd: ORD[Float] =
    cats.kernel.instances.float.catsKernelStdOrderForFloat
  implicit def byteOrd: ORD[Byte] =
    cats.kernel.instances.byte.catsKernelStdOrderForByte
  implicit def shortOrd: ORD[Short] =
    cats.kernel.instances.short.catsKernelStdOrderForShort
  implicit def stringOrd: ORD[String] =
    cats.kernel.instances.string.catsKernelStdOrderForString

  implicit def fromOrdering[T](implicit ordering: Ordering[T]) =
    Order.fromOrdering(ordering)

  implicit def tuple1[T: ORD] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple1[T]
  implicit def tuple2[T1: ORD, T2: ORD]: ORD[(T1, T2)] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple2[T1, T2]
  implicit def tuple3[T1: ORD, T2: ORD, T3: ORD] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple3[T1, T2, T3]
  implicit def tuple4[T1: ORD, T2: ORD, T3: ORD, T4: ORD] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple4[T1, T2, T3, T4]
  implicit def tuple5[T1: ORD, T2: ORD, T3: ORD, T4: ORD, T5: ORD] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple5[T1, T2, T3, T4, T5]
  implicit def tuple6[T1: ORD, T2: ORD, T3: ORD, T4: ORD, T5: ORD, T6: ORD] =
    cats.kernel.instances.tuple
      .catsKernelStdOrderForTuple6[T1, T2, T3, T4, T5, T6]
  implicit def tuple7[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD
  ] =
    cats.kernel.instances.tuple
      .catsKernelStdOrderForTuple7[T1, T2, T3, T4, T5, T6, T7]
  implicit def tuple8[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD
  ] =
    cats.kernel.instances.tuple
      .catsKernelStdOrderForTuple8[T1, T2, T3, T4, T5, T6, T7, T8]
  implicit def tuple9[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD
  ] =
    cats.kernel.instances.tuple
      .catsKernelStdOrderForTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]
  implicit def tuple10[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD
  ] =
    cats.kernel.instances.tuple
      .catsKernelStdOrderForTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
  implicit def tuple11[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD,
      T11: ORD
  ] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple11[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11
    ]
  implicit def tuple12[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD,
      T11: ORD,
      T12: ORD
  ] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple12[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12
    ]
  implicit def tuple13[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD,
      T11: ORD,
      T12: ORD,
      T13: ORD
  ] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple13[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13
    ]
  implicit def tuple14[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD,
      T11: ORD,
      T12: ORD,
      T13: ORD,
      T14: ORD
  ] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple14[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14
    ]
  implicit def tuple15[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD,
      T11: ORD,
      T12: ORD,
      T13: ORD,
      T14: ORD,
      T15: ORD
  ] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple15[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15
    ]
  implicit def tuple16[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD,
      T11: ORD,
      T12: ORD,
      T13: ORD,
      T14: ORD,
      T15: ORD,
      T16: ORD
  ] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple16[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16
    ]
  implicit def tuple17[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD,
      T11: ORD,
      T12: ORD,
      T13: ORD,
      T14: ORD,
      T15: ORD,
      T16: ORD,
      T17: ORD
  ] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple17[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17
    ]
  implicit def tuple18[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD,
      T11: ORD,
      T12: ORD,
      T13: ORD,
      T14: ORD,
      T15: ORD,
      T16: ORD,
      T17: ORD,
      T18: ORD
  ] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple18[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18
    ]
  implicit def tuple19[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD,
      T11: ORD,
      T12: ORD,
      T13: ORD,
      T14: ORD,
      T15: ORD,
      T16: ORD,
      T17: ORD,
      T18: ORD,
      T19: ORD
  ] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple19[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19
    ]
  implicit def tuple20[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD,
      T11: ORD,
      T12: ORD,
      T13: ORD,
      T14: ORD,
      T15: ORD,
      T16: ORD,
      T17: ORD,
      T18: ORD,
      T19: ORD,
      T20: ORD
  ] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple20[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20
    ]
  implicit def tuple21[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD,
      T11: ORD,
      T12: ORD,
      T13: ORD,
      T14: ORD,
      T15: ORD,
      T16: ORD,
      T17: ORD,
      T18: ORD,
      T19: ORD,
      T20: ORD,
      T21: ORD
  ] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple21[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      T21
    ]
  implicit def tuple22[
      T1: ORD,
      T2: ORD,
      T3: ORD,
      T4: ORD,
      T5: ORD,
      T6: ORD,
      T7: ORD,
      T8: ORD,
      T9: ORD,
      T10: ORD,
      T11: ORD,
      T12: ORD,
      T13: ORD,
      T14: ORD,
      T15: ORD,
      T16: ORD,
      T17: ORD,
      T18: ORD,
      T19: ORD,
      T20: ORD,
      T21: ORD,
      T22: ORD
  ] =
    cats.kernel.instances.tuple.catsKernelStdOrderForTuple22[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      T21,
      T22
    ]

}
