import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import math._
import scala.annotation.target.field
import org.scalatest._
import scala.virtualization.lms.common.ScalaOpsPkgExp
import lms.IntegerOpsPower
import lms.TruffleLMS

class TruffleLMSTestPower extends FunSuite with TruffleLMS with IntegerOpsPower {

  runtime = Truffle.getRuntime();
  frameDescriptor = new FrameDescriptor();

  /* 
    Now we implement the posterchild of staging examples:
    specializing the `power` function to a fixed exponent.

    The Truffle AST contains a function specialized for
    exponent 6.
  */    
  test("power") {
    val truffelized = lms { x: Rep[Int] =>

      def power(x: Rep[Int], y: Int): Rep[Int] = {
        if (y == 0) lift(1) else x * power(x, y - 1)
      }
      power(x, 6)
    }

    val result = truffelized(2)
    assert(result === 64);

    assert(truffelized.rootNode.block.toString ===
      """Assign([0,x0,Int],GetArg(0))
Assign([1,x1,Int],IntTimes(Sym([0,x0,Int]),Sym([0,x0,Int])))
Assign([2,x2,Int],IntTimes(Sym([0,x0,Int]),Sym([1,x1,Int])))
Assign([3,x3,Int],IntTimes(Sym([0,x0,Int]),Sym([2,x2,Int])))
Assign([4,x4,Int],IntTimes(Sym([0,x0,Int]),Sym([3,x3,Int])))
Assign([5,x5,Int],IntTimes(Sym([0,x0,Int]),Sym([4,x4,Int])))""")
  }

  test("power2") {
    val truffelized = lms { x: Rep[Int] =>

      def power(b: Rep[Int], x: Int): Rep[Int] = {
        if (x == 0) lift(1)
        else if ((x % 2) == 0) { power(b, x / 2) * power(b, x / 2) }
        else b * power(b, x - 1)
      }
      power(x, 8)
    }
    println(truffelized.rootNode.block.toString);
    val result = truffelized(2)
    assert(result === 256);
  }

  /* 
    The previous example required us to generate a specialized
    AST ahead of time.

    Now we use Truffle's AST rewriting facilities to specialize
    on the fly.

    We introduce a generic Speculate node, that takes a 2-argument
    function as argument and speculates on the second argument
    being stable.

    The power implementation that speculates on a stable exponent
    is then concisely expressed like this:

        lms(speculate(power))
  */

  test("powerSpeculate") {

    case class Speculate(func: (Rep[Int], Int) => Rep[Int], @(Child @field) x: Rep[Int], @(Child @field) y: Rep[Int]) extends Def[Int] {
      var lastY = 0
      @(Child @field) var body = reify(func(x, lastY))

      def execute(frame: VirtualFrame) = {
        val newY = y.execute(frame)
        if (newY != lastY) {
          CompilerDirectives.transferToInterpreterAndInvalidate() // deoptimize!!
          lastY = newY
          body = reify(func(x, lastY))
        }
        body.execute(frame)
      }
    }

    def speculate(f: (Rep[Int], Int) => Rep[Int]) = (x: Rep[Int], y: Rep[Int]) => reflect(Speculate(f, x, y))

    // 

    def power(x: Rep[Int], y: Int): Rep[Int] = {
      if (y == 0) 1 else x * power(x, y - 1)
    }

    def time(s: String)(a: => Unit): Unit = {
      val t0 = System.currentTimeMillis
      a
      val t1 = System.currentTimeMillis
      println(s"$s took ${t1 - t0}ms")
    }
    
    val truffelized = lms[Int, Int, Int] { (x, y) => speculate(power)(x, y) }

    val result = truffelized(2, 6)
    assert(result === 64);

    // trigger compilation for y = 4

    time("No compilation") {
      val result = truffelized(2, 1024);
    }

    for (i <- 0 until 1000000)
      truffelized(i, 1024)

    time("With compilation") {
      val result = truffelized(2, 1014);
    }

    // should be compiled now -- trigger deoptimization
    // and recompilation for y = 6

    for (i <- 0 until 1000000)
      truffelized(i, 6)

    assert(truffelized.rootNode.block.toString ===
      """Assign([0,x0,Int],GetArg(0))
Assign([1,x1,Int],GetArg(1))
Assign([2,x2,Int],Speculate(<function2>,Sym([0,x0,Int]),Sym([1,x1,Int])))""")
  }
}
