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

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import scala.annotation.target.field
import org.scalatest._
import scala.collection.mutable.ArrayBuffer

trait IntegerType extends Base with Types {

   case class IntPlus(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) + y.execute(frame)
    }
  }
  case class IntMinus(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) - y.execute(frame)
    }
  }
  case class IntTimes(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) * y.execute(frame)
    }
  }
  case class IntMod(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) % y.execute(frame)
    }
  }
  case class IntDiv(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) / y.execute(frame)
    }
  }
  case class IntEqual(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) == y.execute(frame)
    }
  }
  case class IntLess(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) < y.execute(frame)
    }
  }
  
  def int_plus(x: Exp[Int], y: Exp[Int]): Exp[Int]= reflect(IntPlus(x,y))
  def int_minus(x: Exp[Int], y: Exp[Int]): Exp[Int] = reflect(IntMinus(x,y))
  def int_times(x: Exp[Int], y: Exp[Int]): Exp[Int] = reflect(IntTimes(x,y))
  def int_div(x: Exp[Int], y: Exp[Int]): Exp[Int] = reflect(IntDiv(x,y))
  def int_equal(x: Exp[Int], y: Exp[Int]): Exp[Boolean] = reflect(IntEqual(x,y))
  def int_less(x: Exp[Int], y: Exp[Int]): Exp[Boolean] = reflect(IntLess(x,y))
  def int_mod(x: Exp[Int], y: Exp[Int]): Exp[Int] = reflect(IntMod(x,y))

  implicit class IntOps(x: Exp[Int]) {
    def +(y: Exp[Int]): Exp[Int] = int_plus(x,y)
    def -(y: Exp[Int]): Exp[Int] = int_minus(x,y)
    def *(y: Exp[Int]): Exp[Int] = int_times(x,y)
    def /(y: Exp[Int]): Exp[Int] = int_div(x,y)
    def ===(y: Exp[Int]): Exp[Boolean] = int_equal(x,y)
    def <(y: Exp[Int]): Exp[Boolean] = int_less(x,y)
    def %(y: Exp[Int]): Exp[Int] = int_mod(x,y)
  }
}

trait IntegerOps extends IntegerType {

  override def int_plus(x: Exp[Int], y: Exp[Int])= (x, y) match {
    case (Const(x), Const(y)) => Const(x + y)
    case _ => super.int_plus(x, y)
  }
  
  override def int_minus(x: Rep[Int], y: Rep[Int])= (x, y) match {
    case (Const(x), Const(y)) => Const(x - y)
    case _ => super.int_minus(x, y)
  }
  
  override def int_times(x: Rep[Int], y: Rep[Int])= (x, y) match {
    case (Const(x), Const(y)) => Const(x * y)
    case _ => super.int_times(x, y)
  }
}

trait IntegerOpsPower extends IntegerOps {
  override def int_plus(x: Rep[Int], y: Rep[Int])= (x, y) match {
    case (x, Const(0)) => x
    case (Const(0), y) => y
    case _ => super.int_plus(x, y)
  }
  
  override def int_minus(x: Rep[Int], y: Rep[Int])= (x, y) match {
    case (x, Const(0)) => x
    case _ => super.int_minus(x, y)
  }
  
  override def int_times(x: Rep[Int], y: Rep[Int])= (x, y) match {
    case (x, Const(0)) => Const(0)
    case (Const(0), y) => Const(0)
  	case (x, Const(1)) => x
    case (Const(1), y) => y
    case (x, Const(-1)) => int_minus(Const(0), x)
    case (Const(-1), y) => int_minus(Const(0), y)
    case _ => super.int_times(x, y)
  }
}
