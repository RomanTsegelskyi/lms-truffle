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
import scala.virtualization.lms.common.ScalaOpsPkgExp

class TruffleLMSTestSimple extends FunSuite with TruffleLMS with Trig {

  /* 
    This test demonstrates the core LMS -> Truffle mechanics:
    
    We create a Truffle RootNode instance that takes an LMS Block[T],
    which is also a Truffle Node.

    The LMS block is obtained via `reify(code)`. The reify operation 
    collects all statements created inside its dynamic scope. The body 
    of `code` uses `lift` to convert Int values to Rep[Int].

    The result AST is equivalent to `x0 = 20 + 22; x0`.
  */

  test("constants") {
    class TestRootNode[T](desc: FrameDescriptor, @(Child @field) var block: Block[T]) extends RootNode(null, desc) {
      override def execute(frame: VirtualFrame): AnyRef = block.execute(frame).asInstanceOf[AnyRef]
    }

    def code = {
      lift(20) + lift(22)
    }

    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();
    var rc = reify(code);
    val rootNode = new TestRootNode(frameDescriptor, rc);
    val target: CallTarget = runtime.createCallTarget(rootNode);
    val result = target.call();
    assert(result === 42);

    assert(rootNode.block.toString ===
      """Assign([0,x0,Int],IntPlus(Const(20),Const(22)))""")
  }

  /* 
    Here we do not create a RootNode explicitly but rely on the
    `lms` function, which will create one for us.
    
    Function arguments are also handled internally. We could have 
    dropped the explicit `lift` call, as value 22 would be implicitly 
    converted to Rep[Int].
  */

  test("arguments") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();
    val truffelized = lms { x: Rep[Int] =>
      x + lift(22)
    }

    val result = truffelized(20)
    assert(result === 42);

    assert(truffelized.rootNode.block.toString ===
      """Assign([0,x0,Int],GetArg(0))
Assign([1,x1,Int],IntPlus(Sym([0,x0,Int]),Const(22)))""")

  }
  
  
  test("tri functions"){
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();
    val truffelized = lms { x: Rep[Double] =>
      cos(x)
    }

    val result = truffelized(0.5)
    assert(result === 0.8775825618903728);

  }
}
