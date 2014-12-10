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


trait ControlFlow extends Primitives with Base {

  case class Loop(shy: Boolean, @(Child @field)body: Block[Boolean]) extends Def[Boolean] {
    def execute(frame: VirtualFrame): Boolean = {
      //println(s"interpreted: ${CompilerDirectives.inInterpreter}")
      //while(body.execute(frame)) {}
      while ({
        if (CompilerAsserts.compilationConstant(shy && CompilerDirectives.inInterpreter)) {
          if (body.execute(frame)) return true else false
        } else {
          body.execute(frame)
        }
      }) { }
      false
    }
  }

  def loop(body: => Exp[Boolean]): Exp[Boolean] = reflect(Loop(false,reify(body)))

  def loopShy(body: => Exp[Boolean]): Exp[Boolean] = reflect(Loop(true,reify(body)))

  case class IfElse[@specialized T](c: Block[Boolean], a: Block[T], b: Block[T]) extends Def[T] {
    def execute(frame: VirtualFrame): T = {
      if (c.execute(frame)) a.execute(frame) else b.execute(frame)
    }
  }

  def cond[T:Typ](c: => Exp[Boolean])(a: => Exp[T])(b: => Exp[T]): Exp[T] = {
    reflect(IfElse(reify(c), reify(a), reify(b)))
  }
  
  case class WhileLoop[@specialized T](cond : Block[Boolean], @(Child @field)body: Block[T]) extends Def[Unit] {
    def execute(frame: VirtualFrame) = {
      while (cond.execute(frame)) {
    	  body.execute(frame)
      }
    }
  }
  
  def whileloop[T:Typ](c: => Exp[Boolean])(b: => Exp[T]): Exp[Unit] = {
    reflect(WhileLoop(reify(c), reify(b)))
  }
  
}
