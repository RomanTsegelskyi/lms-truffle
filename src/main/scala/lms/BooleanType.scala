package lms

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import scala.annotation.target.field
import org.scalatest._

trait BooleanType extends Base with Types {

  case class BooleanAnd(@(Child @field) x: Exp[Boolean], @(Child @field) y: Exp[Boolean]) extends Def[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) && y.execute(frame)
    }
  }

  case class BooleanNot(@(Child @field) x: Exp[Boolean]) extends Def[Boolean] {
    def execute(frame: VirtualFrame) = {
      !x.execute(frame)
    }
  }

  def boolean_and(x: Exp[Boolean], y: Exp[Boolean]): Exp[Boolean] = x match {
    case Const(true) => y
    case Const(false) => false
    case _ => reflect(BooleanAnd(x, y))
  }
  
  def boolean_not(x: Exp[Boolean]): Exp[Boolean] =  x match {
    case Const(true) => false
    case Const(false) => true
    case _ => reflect(BooleanNot(x))
  }

  implicit class BooleanOps(x: Exp[Boolean]) {
    def &&(y: Exp[Boolean]): Exp[Boolean] = boolean_and(x, y)
    def unary_!(): Exp[Boolean] = boolean_not(x)
  }

}