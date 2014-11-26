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

trait DoubleType extends Base with Types {

   case class DoublePlus(@(Child @field) x: Exp[Double], @(Child @field) y: Exp[Double]) extends Def[Double] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) + y.execute(frame)
    }
  }

  case class DoubleMinus(@(Child @field) x: Exp[Double], @(Child @field) y: Exp[Double]) extends Def[Double] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) - y.execute(frame)
    }
  }

  case class DoubleTimes(@(Child @field) x: Exp[Double], @(Child @field) y: Exp[Double]) extends Def[Double] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) * y.execute(frame)
    }
  }

  case class DoubleDiv(@(Child @field) x: Exp[Double], @(Child @field) y: Exp[Double]) extends Def[Double] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) / y.execute(frame)
    }
  }
  
  def double_plus(x: Exp[Double], y: Exp[Double]): Exp[Double] = reflect(DoublePlus(x,y))
  def double_minus(x: Exp[Double], y: Exp[Double]): Exp[Double] = reflect(DoubleMinus(x,y))
  def double_times(x: Exp[Double], y: Exp[Double]): Exp[Double] = reflect(DoubleTimes(x,y))
  def double_div(x: Exp[Double], y: Exp[Double]): Exp[Double] = reflect(DoubleDiv(x,y))

  implicit class DoubleOps(x: Exp[Double]) {
    def +(y: Exp[Double]): Exp[Double] = double_plus(x, y)
    def -(y: Exp[Double]): Exp[Double] = double_minus(x, y)
    def *(y: Exp[Double]): Exp[Double] = double_times(x, y)
    def /(y: Exp[Double]): Exp[Double] = double_div(x, y)
  }

}

trait DoubleOps extends DoubleType {

  override def double_plus(x: Exp[Double], y: Exp[Double])= (x, y) match {
    case (Const(x), Const(y)) => Const(x + y)
    case _ => super.double_plus(x, y)
  }
  
  override def double_minus(x: Rep[Double], y: Rep[Double])= (x, y) match {
    case (Const(x), Const(y)) => Const(x - y)
    case _ => super.double_minus(x, y)
  }
  
  override def double_times(x: Rep[Double], y: Rep[Double])= (x, y) match {
    case (Const(x), Const(y)) => Const(x * y)
    case _ => super.double_times(x, y)
  }
}

trait DoubleOpsFFT extends DoubleOps {
  override def double_plus(x: Rep[Double], y: Rep[Double])= (x, y) match {
    case (x, Const(0.0)) => x
    case (Const(0.0), y) => y
    case _ => super.double_plus(x, y)
  }
  
  override def double_minus(x: Rep[Double], y: Rep[Double])= (x, y) match {
    case (x, Const(0.0)) => x
    case _ => super.double_minus(x, y)
  }
  
  override def double_times(x: Rep[Double], y: Rep[Double])= (x, y) match {
    case (x, Const(0.0)) => Const(0.0)
    case (Const(0.0), y) => Const(0.0)
  	case (x, Const(1.0)) => x
    case (Const(1.0), y) => y
    case (x, Const(-1.0)) => double_minus(Const(0.0), x)
    case (Const(-1.0), y) => double_minus(Const(0.0), y)
    case _ => super.double_times(x, y)
  }
}
