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

package ffttest

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import org.scalatest._
import fft.FFT
import lms.TruffleLMS
import lms.TrigOptFFT
import lms.DoubleOpsFFT

/* 
  Scala adaptation of Truffle API Test Suite

  see:  http://hg.openjdk.java.net/graal/graal/file/483d05bf77a7/graal/com.oracle.truffle.api.test/src/com/oracle/truffle/api/test
*/

class TruffleLMSFFT extends FunSuite with FFT with TruffleLMS with TrigOptFFT with DoubleOpsFFT {

  def time(s: String)(a: => Unit): Unit = {
    val t0 = System.currentTimeMillis
    a
    val t1 = System.currentTimeMillis
    println(s"$s took ${t1 - t0}ms")
  }

  test("fft size 4") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();

    val truffelized = lms { x: Rep[Array[Double]] =>
      val y = NewArray[Double](8);
      val inList = scala.List.tabulate(4)(i => Complex(x(2 * i), x(2 * i + 1)))
      val outList = fft(inList)
      for (i <- 0 to 3) {
        y(2 * i) = outList(i).re;
        y(2 * i + 1) = outList(i).im;
      }
      y
    }

    val res = truffelized(Array(1.0, 0.0, 1.0, 0.0, 2.0, 0.0, 2.0, 0.0));
    assert(res === Array(6.0, 0.0, -1.0, 1.0, 0.0, 0.0, -1.0, -1.0))
    println(truffelized.rootNode.block.toString)
  }

  test("fft any size") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();

    def fftc(size: Int) = {
      val truffelized = lms { x: Rep[Array[Double]] =>
        val y = NewArray[Double](2 * size)
        val inList = scala.List.tabulate(size)(i => Complex(x(2 * i), x(2 * i + 1)))
        println(inList)
        val outList = fft(inList)
        println(outList)
        for (i <- 0 to (size - 1)) {
          y(2 * i) = outList(i).re;
          y(2 * i + 1) = outList(i).im;
        }
        y
      }
      truffelized
    }
    val fft32 = fftc(32);
    val arr = Array(1.0, 0.0, 1.0, 0.0, 2.0, 0.0, 2.0, 0.0);
    val arrE = Seq.fill(8)(arr).flatten.toArray;
    val res = fft32(arrE);
    println(fft32.rootNode.block.toString)
  }
}

