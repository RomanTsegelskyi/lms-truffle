
package lmstest

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import org.scalatest._
import lms.TruffleLMS

class TruffleLMSTestArray extends FunSuite with TruffleLMS {

  test("simple access") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();

    val truffelized = lms { x: Rep[Array[Int]] =>
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

    val truffelized = lms { x: Rep[Array[Int]] =>
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

    val truffelized = lms { x: Rep[Array[Int]] =>
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

    val truffelized = lms { x: Rep[Array[Double]] =>
      val y = x(0) + x(1)
      y
    }

    var arr = Array(20.2, 22.2);
    val result = truffelized(arr)
    println(truffelized.rootNode.block.toString)
    assert(result === 42.4);
  }

  test("new array") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();

    val truffelized = lms { x: Rep[Array[Double]] =>
      val y = NewArray[Double](2)
      y(0) = x(0) + lift(1)
      y(1) = x(1) + lift(1)
      y
    }

    var arr = Array(20.2, 22.2);
    val result = truffelized(arr)
    println(truffelized.rootNode.block.toString)
    assert(result === Array(21.2, 23.2));

  }
}

