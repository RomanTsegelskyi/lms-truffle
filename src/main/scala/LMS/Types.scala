/*
 * Copyright (c) 2014, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package LMS

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import org.scalatest._

trait Types extends Base {

  implicit object unitTyp extends Typ[Unit]{
    def slotKind = FrameSlotKind.Object
  }
  
  implicit object intTyp extends Typ[Int] {
    def slotKind = FrameSlotKind.Int
  }
  implicit object boolTyp extends Typ[Boolean] {
    def slotKind = FrameSlotKind.Boolean
  }

  implicit object doubleTyp extends Typ[Double] {
    def slotKind = FrameSlotKind.Double
  }
  
  implicit object stringTyp extends Typ[String] {
    def slotKind = FrameSlotKind.Object
  }
  
  // probably something here
  implicit def arrayTyp[T:Typ] = new Typ[Array[T]] {
    def slotKind = FrameSlotKind.Object
  }

}
