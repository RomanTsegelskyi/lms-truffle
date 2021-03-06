package lmstest

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import math._
import org.scalatest._
import lms.TruffleLMS


class TestTruffleLMS2 extends FunSuite with TruffleLMS {

  runtime = Truffle.getRuntime();
  frameDescriptor = new FrameDescriptor();

  /*
  loop pattern: while (C) becomes

  val loop = function {
    while {
      val r = C
      if (CompilerDirectives.inInterpreter) return true
      r
    }
    false
  }

  while {
    if (CompilerDirectives.inInterpreter) 
      loop.call(...)
    else
      loop.rootNode.block.execute(...) // inline if parent is compiled, too
  }
*/

  test("loopOSR") {

    def time(s: String)(a: => Unit): Unit = {
      val t0 = System.currentTimeMillis
      a
      val t1 = System.currentTimeMillis
      println(s"$s took ${t1 - t0}ms")
    }

    val plain1 = { (x: Int, y: Int) =>
      var w = 0
      var i = 0

      do {
        w = w + (if (0 < x) { x * y / x / y * x * y } else { 0 })
        i = i + 1
      } while (i < (10 * 1000000))
      w
    }

    val plain = lms[Int, Int, Int] { (x0, y0) =>

      case class FF() extends Exp[Int] {
        def execute(frame: VirtualFrame): Int = {
          val x = x0.execute(frame)
          val y = y0.execute(frame)
          var w = 0
          var i = 0

          do {
            w = w + (if (0 < x) { x * y / x / y * x * y } else { 0 })
            i = i + 1
          } while (i < (10 * 1000000))

          w
        }
      }
      FF()
    }
    println(plain.rootNode.block.toString)
    val truffelized = lms[Int, Int, Int] { (x, y) =>
      val w = cell(0)
      val i = cell(0)

      val f = fun { () =>
        loopShy {
          w() = w() + (cond(lift(0) < x) { x * y / x / y * x * y } { 0 })
          i() = i() + 1
          i() < (10 * 1000000)
        }
      }

      loop {
        f()
      }
      w()
    }


//    time("A") {
//      val result = truffelized(2, 6)
//      assert(result === 120 * 1000000);
//    }
//
//    time("B") {
//      for (i <- 0 until 10)
//        truffelized(i, 4)
//    }
//
//    time("C") {
//      for (i <- 0 until 10)
//        truffelized(i, 6)
//    }
//
//    var z = 0
//    time("U") {
//      val result = plain(2, 6)
//      assert(result === 120 * 1000000);
//    }
//
//    time("V") {
//      for (i <- 0 until 10)
//        z += plain(i, 4)
//    }
//
//    time("W") {
//      for (i <- 0 until 10)
//        z += plain(i, 6)
//    }

        println(truffelized.rootNode.block.toString)

    /*

example timings:

[truffle] opt queued       LMSNestedRootNode...
[truffle] opt start        LMSNestedRootNode...
[truffle] opt done         LMSNestedRootNode...
A took 218ms
[truffle] opt queued       LMSRootNode@318aaf4d 
[truffle] opt start        LMSRootNode@318aaf4d 
[truffle] opt done         LMSRootNode@318aaf4d 
B took 1061ms
C took 984ms
U took 51ms
[truffle] opt queued       LMSRootNode@1e2b0c8d      
[truffle] opt start        LMSRootNode@1e2b0c8d      
[truffle] opt done         LMSRootNode@1e2b0c8d      
V took 489ms
W took 495ms

*/

    //    println(z)
    //    assert(truffelized.rootNode.block.toString === 
    //"""Assign([0,x0,Int],GetArg(0))
    //Assign([1,x1,Int],GetArg(1))
    //Assign([2,x2,Int],Read(Const(0)))
    //Assign([3,x3,Int],Read(Const(0)))
    //Assign([19,x19,Boolean],Loop(false,Assign([18,x18,Boolean],LMSNestedCallNode(LMSNestedRootNode@XX <compiled>,LMSNestedRootNode@XX))))
    //Assign([20,x20,Int],Read(Sym([2,x2,Int])))""")  
  }
}