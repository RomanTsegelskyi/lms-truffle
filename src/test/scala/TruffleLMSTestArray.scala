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
import LMS.TruffleLMS

/* 
  Scala adaptation of Truffle API Test Suite

  see:  http://hg.openjdk.java.net/graal/graal/file/483d05bf77a7/graal/com.oracle.truffle.api.test/src/com/oracle/truffle/api/test
*/

class TruffleLMSTestArray extends FunSuite with TruffleLMS {

  test("simple access") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();
    
    val truffelized = lms {x:Rep[Array[Int]] =>
    	x(0) + x(1)
    }

    var arr = Array(20, 22);
    val result = truffelized(arr)
    println(truffelized.rootNode.block.toString)
    assert(result === 42);

  }
  
  test("simple assignment") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();
    
    val truffelized = lms {x:Rep[Array[Int]] =>
    	x(0) = 1;
    	x(0)
    }

    var arr = Array(20, 22);
    val result = truffelized(arr)
    println(truffelized.rootNode.block.toString)
    assert(result === 1);
  }

  
  test("sum array") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();
    
    val truffelized = lms {x:Rep[Array[Int]] =>
    	val y = x(0) + x(1)
    	y
    }

    var arr = Array(20, 22);
    val result = truffelized(arr)
    println(truffelized.rootNode.block.toString)
    assert(result === 42);
  }

  
   test("sum array double") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();
    
    val truffelized = lms {x:Rep[Array[Double]] =>
    	val y = x(0) + x(1)
    	y
    }

    var arr = Array(20.2, 22.2);
    val result = truffelized(arr)
    println(truffelized.rootNode.block.toString)
    assert(result === 42.4);
  }
   
   test("new array"){
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();
    
    val truffelized = lms {x:Rep[Array[Double]] =>
    	val y = NewArray[Double](2)
    	y(0) = x(0) + lift(1)
    	y(1) = x(1) + lift(1)
    }

    var arr = Array(20.2, 22.2);
    val result = truffelized(arr)
    println(truffelized.rootNode.block.toString)
    assert(result === Array(21.2, 23.2));
     
   }

}

