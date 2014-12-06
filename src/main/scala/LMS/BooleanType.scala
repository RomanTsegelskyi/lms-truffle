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


trait BooleanType extends Base with Types {

  case class BooleanEq(@(Child @field) x: Exp[Boolean], @(Child @field) y: Exp[Boolean]) extends Def[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) && y.execute(frame)
    }
  }

  case class BooleanAnd(@(Child @field) x: Exp[Boolean], @(Child @field) y: Exp[Boolean]) extends Def[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) && y.execute(frame)
    }
  }

  def boolean_eq(x: Exp[Boolean], y: Exp[Boolean]): Exp[Boolean] = reflect(BooleanEq(x, y))
  def boolean_and(x: Exp[Boolean], y: Exp[Boolean]): Exp[Boolean] = reflect(BooleanAnd(x, y))

  implicit class BooleanOps(x: Exp[Boolean]) {
    def ===(y: Exp[Boolean]): Exp[Boolean] = boolean_eq(x, y)
    def &&(y: Exp[Boolean]): Exp[Boolean] = boolean_and(x, y)
  }

}