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
package org.saddle.scalar

import org.saddle.ORD
import org.saddle.array.Sorter

/** Short ScalarTag
  */
object ScalarTagShort extends ScalarTagAny[Short] {
  override def parse(s: String) = s.toByte
  override def makeSorter(implicit ord: ORD[Short]): Sorter[Short] =
    Sorter.shortSorter

  override def missing: Short = Short.MinValue
}
