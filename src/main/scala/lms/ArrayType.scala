package lms

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import scala.annotation.target.field
import org.scalatest._

trait ArrayType extends Base with Types {

  case class ArrayRead[T](@(Child @field) arr: Exp[Array[T]], @(Child @field) x: Exp[Int]) extends Def[T] {
    def execute(frame: VirtualFrame) = {
      val index = x.execute(frame)
      val res = arr.execute(frame)(index)
      res
    }
  }

  case class ArrayUpdate[T:Typ](@(Child @field) arr: Exp[Array[T]],
    @(Child @field) index: Exp[Int],
    @(Child @field) element: Exp[T]) extends Def[Unit] {
    def execute(frame: VirtualFrame) = {
      val array = arr.execute(frame)
      array(index.execute(frame)) = element.execute(frame)
    }
  }
  
  case class ArrayNew[T:Manifest](@(Child @field) size: Exp[Int]) extends Def[Array[T]] {
    def execute(frame: VirtualFrame) = {
      val s = size.execute(frame);
      new Array[T](s);
    }
  }

  // Is it possible to have a unified update class
  implicit class ArrayOps[T:Typ](x: Exp[Array[T]]) {
    def apply(y: Exp[Int]): Exp[T] = reflect(ArrayRead[T](x, y))
    def update(y: Exp[Int], e: Exp[T]): Exp[Unit] = reflect(ArrayUpdate[T](x, y, e)) 
  }

  object NewArray {
    def apply[T:Typ:Manifest](n: Exp[Int]) : Exp[Array[T]] = reflect(ArrayNew[T](n))
  }
}
