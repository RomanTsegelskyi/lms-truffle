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
import com.oracle.truffle.api.frame._;
import com.oracle.truffle.api.nodes._;
import com.oracle.truffle.api.nodes.Node._;

import scala.annotation.target.field

import org.scalatest._

/* 
  Scala adaptation of Truffle API Test Suite

  see:  http://hg.openjdk.java.net/graal/graal/file/483d05bf77a7/graal/com.oracle.truffle.api.test/src/com/oracle/truffle/api/test
*/

class TruffleArrayTest extends FunSuite {

    class GetArrayElementNode(array: CallTarget, index: CallTarget) extends RootNode {
      override def execute(frame: VirtualFrame): Object = {
        val arr = array.call().asInstanceOf[Array[Object]];
      	arr(index.call().asInstanceOf[Int]);
      }
    }
    
    class SetArrayElementNode(array: CallTarget, index: CallTarget, element: CallTarget) extends RootNode {
      override def execute(frame: VirtualFrame) : Array[Object]= {
        var arr = array.call().asInstanceOf[Array[Object]];
      	arr(index.call().asInstanceOf[Int]) = element.call();
      	arr
      }
    }
    
    class ArrayNode(value: Array[Object]) extends RootNode {
        override def execute(frame: VirtualFrame): Array[Object] = value
    }
    
    class ConstantRootNode(value: Int) extends RootNode {
        override def execute(frame: VirtualFrame): Integer = value
    }

  test("ArrayGetElementTest") {

    val runtime = Truffle.getRuntime();
    var arr = Array(new java.lang.Integer(20), new java.lang.Integer(22));
    val foo = runtime.createCallTarget(new ArrayNode(arr.asInstanceOf[Array[Object]]));
    val bar = runtime.createCallTarget(new ConstantRootNode(1));
    val main = runtime.createCallTarget(new GetArrayElementNode(foo, bar));
    val result = main.call();
    assert(22 === result);
  }
  
  test("ArraySetElementTest") {

    val runtime = Truffle.getRuntime();
    var arr = Array(new java.lang.Integer(20), new java.lang.Integer(22));
    val a = runtime.createCallTarget(new ArrayNode(arr.asInstanceOf[Array[Object]]));
    val i = runtime.createCallTarget(new ConstantRootNode(1));
    val e = runtime.createCallTarget(new ConstantRootNode(15));
    val main = runtime.createCallTarget(new SetArrayElementNode(a, i, e));
    val result = main.call();
    assert(15 === result.asInstanceOf[Array[Object]](1));
  }

}

