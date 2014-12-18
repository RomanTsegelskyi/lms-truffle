package lmstest

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import org.scalatest._
import lms.DoubleOps
import lms.TruffleLMS

class TruffleLMSOpt extends FunSuite with TruffleLMS with DoubleOps {

  test("plus opt") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();
    
    val truffelized = lms {x:Rep[Double] =>
    	lift(1.0) + lift(2) + x
    }

    val res = truffelized(2.0);
    assert(res === 5.0)
  }

}

