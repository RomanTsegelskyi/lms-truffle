package truffletest

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import org.scalatest._

class TruffleTestArray extends FunSuite {

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

