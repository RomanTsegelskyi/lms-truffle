package lms

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import scala.annotation.target.field
import org.scalatest._

trait StringType extends Base with Types {

  case class StringPlus(@(Child @field) x: Exp[String], @(Child @field) y: Exp[String]) extends Def[String] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) + y.execute(frame)
    }
  }

  case class StringEq(@(Child @field) x: Exp[String], @(Child @field) y: Exp[String]) extends Def[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) == y.execute(frame)
    }
  }

  case class StringHashCode(@(Child @field) x: Exp[String]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame).hashCode().intValue()
    }
  }

  case class StringToInt(@(Child @field) x: Exp[String]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame).toInt
    }
  }

  def string_plus(x: Exp[String], y: Exp[String]): Exp[String] = reflect(StringPlus(x, y))
  def string_equals(x: Exp[String], y: Exp[String]): Exp[Boolean] = reflect(StringEq(x, y))
  def string_hashcode(x: Exp[String]): Exp[Int] = reflect(StringHashCode(x))
  def string_toint(x: Exp[String]): Exp[Int] = reflect(StringToInt(x))

  implicit class StringOps(x: Rep[String]) {
    def +(y: Exp[String]): Exp[String] = string_plus(x, y)
    def ===(y: Exp[String]): Exp[Boolean] = string_equals(x, y)
    def HashCode(): Exp[Int] = string_hashcode(x)
    def toInt(): Exp[Int] = string_toint(x)

  }

}
