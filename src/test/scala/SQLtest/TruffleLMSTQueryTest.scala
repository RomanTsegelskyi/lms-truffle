package SQLtest
import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import scala.annotation.target.field
import org.scalatest._
import FFT.FFT
import LMS.TruffleLMS
import SQL._

class SQLTruffleLMS extends FunSuite with TruffleLMS with SQLParser with StagedQueryProcessor with query_staged0.QueryCompiler{

  def time(s: String)(a: => Unit): Unit = {
    val t0 = System.currentTimeMillis
    a
    val t1 = System.currentTimeMillis
    println(s"$s took ${t1 - t0}ms")
  }
  
  case class PrintFields(fields: Fields) extends Def[Unit] {
    def execute(frame: VirtualFrame) = {
      val f = fields.map{x=>x.execute(frame)}
      printf(f.map{_ => "%s"}.mkString("",  ','.toString, "\n"), f:_*)
    }
  }

  test("fft size 4") {
    runtime = Truffle.getRuntime();
    frameDescriptor = new FrameDescriptor();

    val truffelized = lms { filename: Rep[String] =>
    	
    }

    val res = truffelized("src/data/t.csv");
//    println(res)
//    assert(res === "abc123")
    println(truffelized.rootNode.block.toString)
  }
}

