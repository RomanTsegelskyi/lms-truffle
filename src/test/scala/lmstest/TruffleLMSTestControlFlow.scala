package lmstest

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import org.scalatest._
import lms.TruffleLMS

class TruffleLMSTestControlFlow extends FunSuite with TruffleLMS {

  test("simple loop") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();

    val t = lms { x: Rep[Unit] =>
      val i = cell(0)
      whileloop(i() < 10) {
        i.update(i() + 1)
      }
      i()
    }
    println(t.rootNode.block)
    val res = t();
    println(res)
    

  }
}

