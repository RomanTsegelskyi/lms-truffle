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
import scala.annotation.target.field
import org.scalatest._

trait ArrayType extends Base with Types {

  case class ArrayRead[T](@(Child @field) arr: Exp[Array[T]], @(Child @field) x: Exp[Int]) extends Def[T] {
    def execute(frame: VirtualFrame) = {
      val index = x.execute(frame)
      println("index = " + index)
      val res = arr.execute(frame)(index)
      println(res)
      res
    }
  }

  case class ArrayUpdate[T:Typ](@(Child @field) arr: Exp[Array[T]],
    @(Child @field) index: Exp[Int],
    @(Child @field) element: Exp[T]) extends Def[Unit] {
    def execute(frame: VirtualFrame) = {
      val array = arr.execute(frame)
      array(index.execute(frame)) = element.execute(frame)
    }
  }
  
  case class ArrayNew[T:Manifest](@(Child @field) size: Exp[Int]) extends Def[Array[T]] {
    def execute(frame: VirtualFrame) = {
      val s = size.execute(frame);
      new Array[T](s);
    }
  }

  // Is it possible to have a unified update class
  implicit class ArrayOps[T:Typ](x: Exp[Array[T]]) {
    def apply(y: Exp[Int]): Exp[T] = reflect(ArrayRead[T](x, y))
    def update(y: Exp[Int], e: Exp[T]): Exp[Unit] = reflect(ArrayUpdate[T](x, y, e)) 
  }

  object NewArray {
    def apply[T:Typ:Manifest](n: Exp[Int]) : Exp[Array[T]] = reflect(ArrayNew[T](n))
  }
}
