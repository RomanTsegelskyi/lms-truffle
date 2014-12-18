package lmstest

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import org.scalatest._
import lms.TruffleLMS

class TruffleLMSTestVar extends FunSuite with TruffleLMS {
  
  test("simple assignment") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();
    val truffelized = lms {x:Rep[Int] =>
    	val y = cell(2);
    	y.update(int_plus(y(), 1));
    	y()
    }

    val result = truffelized(5)
    println(truffelized.rootNode.block.toString)
    assert(result === 3);
  }
}

