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
import math._
import scala.annotation.target.field
import org.scalatest._
import scala.virtualization.lms.common.ScalaOpsPkgExp

class TruffleLMSTestPower extends FunSuite with TruffleLMS {

  /* 
    Now we implement the posterchild of staging examples:
    specializing the `power` function to a fixed exponent.

    The Truffle AST contains a function specialized for
    exponent 6.
  */

  test("power") {
    val truffelized = lms { x: Rep[Int] =>

      def power(x: Rep[Int], y: Int): Rep[Int] = {
        if (y == 0) 1 else x * power(x, y - 1)
      }
      power(x, 6)
    }

    val result = truffelized(2)
    assert(result === 64);

    assert(truffelized.rootNode.block.toString ===
      """Assign([0,x0,Int],GetArg(0))
Assign([1,x1,Int],IntTimes(Sym([0,x0,Int]),Const(1)))
Assign([2,x2,Int],IntTimes(Sym([0,x0,Int]),Sym([1,x1,Int])))
Assign([3,x3,Int],IntTimes(Sym([0,x0,Int]),Sym([2,x2,Int])))
Assign([4,x4,Int],IntTimes(Sym([0,x0,Int]),Sym([3,x3,Int])))
Assign([5,x5,Int],IntTimes(Sym([0,x0,Int]),Sym([4,x4,Int])))
Assign([6,x6,Int],IntTimes(Sym([0,x0,Int]),Sym([5,x5,Int])))""")
  }

  test("power2") {
    val truffelized = lms { x: Rep[Int] =>

      def power(b: Rep[Int], x: Int): Rep[Int] = {
        if (x == 0) lift(1)
        else if ((x % 2) == 0) { power(b, x / 2) * power(b, x / 2) }
        else b * power(b, x - 1)
      }
      power(x, 8)
    }

    val result = truffelized(2)
    assert(result === 256);
  }

  /* 
    The previous example required us to generate a specialized
    AST ahead of time.

    Now we use Truffle's AST rewriting facilities to specialize
    on the fly.

    We introduce a generic Speculate node, that takes a 2-argument
    function as argument and speculates on the second argument
    being stable.

    The power implementation that speculates on a stable exponent
    is then concisely expressed like this:

        lms(speculate(power))
  */

  test("powerSpeculate") {

    case class Speculate(func: (Rep[Int], Int) => Rep[Int], @(Child @field) x: Rep[Int], @(Child @field) y: Rep[Int]) extends Def[Int] {
      var lastY = 0
      @(Child @field) var body = reify(func(x, lastY))

      def execute(frame: VirtualFrame) = {
        val newY = y.execute(frame)
        if (newY != lastY) {
          CompilerDirectives.transferToInterpreterAndInvalidate() // deoptimize!!
          lastY = newY
          body = reify(func(x, lastY))
        }
        body.execute(frame)
      }
    }

    def speculate(f: (Rep[Int], Int) => Rep[Int]) = (x: Rep[Int], y: Rep[Int]) => reflect(Speculate(f, x, y))

    // 

    def power(x: Rep[Int], y: Int): Rep[Int] = {
      if (y == 0) 1 else x * power(x, y - 1)
    }

    val truffelized = lms[Int, Int, Int] { (x, y) => speculate(power)(x, y) }

    val result = truffelized(2, 6)
    assert(result === 64);

    // trigger compilation for y = 4

    for (i <- 0 until 1000000)
      truffelized(i, 4)

    // should be compiled now -- trigger deoptimization
    // and recompilation for y = 6

    for (i <- 0 until 1000000)
      truffelized(i, 6)

    assert(truffelized.rootNode.block.toString ===
      """Assign([0,x0,Int],GetArg(0))
Assign([1,x1,Int],GetArg(1))
Assign([2,x2,Int],Speculate(<function2>,Sym([0,x0,Int]),Sym([1,x1,Int])))""")
  }
}
