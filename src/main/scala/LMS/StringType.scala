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

trait StringType extends Base with Types {

  case class StringPlus(@(Child @field) x: Exp[String], @(Child @field) y: Exp[String]) extends Def[String] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) + y.execute(frame)
    }
  }

  case class StringEq(@(Child @field) x: Exp[String], @(Child @field) y: Exp[String]) extends Def[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) == y.execute(frame)
    }
  }
  
  case class StringHashCode(@(Child @field) x: Exp[String]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame).hashCode().intValue()
    }
  }

  def string_plus(x: Exp[String], y: Exp[String]): Exp[String] = reflect(StringPlus(x, y))
  def string_equals(x: Exp[String], y: Exp[String]): Exp[Boolean] = reflect(StringEq(x, y))
  def string_hashcode(x: Exp[String]) : Exp[Int] = reflect(StringHashCode(x))
  
  implicit class StringOps(x: Rep[String]) {
    def +(y: Exp[String]): Exp[String] = string_plus(x, y)
    def ===(y: Exp[String]): Exp[Boolean] = string_equals(x, y)
    def HashCode(): Exp[Int] = string_hashcode(x)

  }

}
