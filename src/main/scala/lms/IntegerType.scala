package lms

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import scala.annotation.target.field
import org.scalatest._

trait IntegerType extends Base with Types {

  case class IntPlus(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) + y.execute(frame)
    }
  }
  case class IntMinus(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) - y.execute(frame)
    }
  }
  case class IntTimes(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) * y.execute(frame)
    }
  }
  case class IntMod(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) % y.execute(frame)
    }
  }
  case class IntDiv(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) / y.execute(frame)
    }
  }
  case class IntEqual(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) == y.execute(frame)
    }
  }
  case class IntNotEqual(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) != y.execute(frame)
    }
  }
  case class IntLess(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) < y.execute(frame)
    }
  }

  case class IntToInt(@(Child @field) x: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame)
    }
  }

  case class IntBitAnd(@(Child @field) x: Exp[Int], @(Child @field) y: Exp[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      val res = x.execute(frame) & y.execute(frame)
      res.asInstanceOf[Int]
    }
  }

  case class IntToString(@(Child @field) x: Exp[Int]) extends Def[String] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame).toString
    }
  }

  def int_plus(x: Exp[Int], y: Exp[Int]): Exp[Int] = reflect(IntPlus(x, y))
  def int_minus(x: Exp[Int], y: Exp[Int]): Exp[Int] = reflect(IntMinus(x, y))
  def int_times(x: Exp[Int], y: Exp[Int]): Exp[Int] = reflect(IntTimes(x, y))
  def int_div(x: Exp[Int], y: Exp[Int]): Exp[Int] = reflect(IntDiv(x, y))
  def int_equal(x: Exp[Int], y: Exp[Int]): Exp[Boolean] = reflect(IntEqual(x, y))
  def int_less(x: Exp[Int], y: Exp[Int]): Exp[Boolean] = reflect(IntLess(x, y))
  def int_mod(x: Exp[Int], y: Exp[Int]): Exp[Int] = reflect(IntMod(x, y))
  def int_toint(x: Exp[Int]): Exp[Int] = reflect(IntToInt(x))
  def int_bit_and(x: Exp[Int], y: Exp[Int]) = reflect(IntBitAnd(x, y))
  def int_not_equal(x: Exp[Int], y: Exp[Int]) = reflect(IntNotEqual(x, y))
  def int_to_string(x: Exp[Int]): Exp[String] = reflect(IntToString(x))

  implicit class IntOps(x: Exp[Int]) {
    def +(y: Exp[Int]): Exp[Int] = int_plus(x, y)
    def -(y: Exp[Int]): Exp[Int] = int_minus(x, y)
    def *(y: Exp[Int]): Exp[Int] = int_times(x, y)
    def /(y: Exp[Int]): Exp[Int] = int_div(x, y)
    def ===(y: Exp[Int]): Exp[Boolean] = int_equal(x, y)
    def <(y: Exp[Int]): Exp[Boolean] = int_less(x, y)
    def %(y: Exp[Int]): Exp[Int] = int_mod(x, y)
    def toInt(): Exp[Int] = int_toint(x)
    def &(y: Exp[Int]): Exp[Int] = int_bit_and(x, y)
    def !=(y: Exp[Int]): Exp[Boolean] = int_not_equal(x, y)
    def ToString(): Exp[String] = int_to_string(x)
  }

}

trait IntegerOps extends IntegerType {

  override def int_plus(x: Exp[Int], y: Exp[Int]) = (x, y) match {
    case (Const(x), Const(y)) => Const(x + y)
    case _ => super.int_plus(x, y)
  }

  override def int_minus(x: Rep[Int], y: Rep[Int]) = (x, y) match {
    case (Const(x), Const(y)) => Const(x - y)
    case _ => super.int_minus(x, y)
  }

  override def int_times(x: Rep[Int], y: Rep[Int]) = (x, y) match {
    case (Const(x), Const(y)) => Const(x * y)
    case _ => super.int_times(x, y)
  }
}

trait IntegerOpsPower extends IntegerOps {
  override def int_plus(x: Rep[Int], y: Rep[Int]) = (x, y) match {
    case (x, Const(0)) => x
    case (Const(0), y) => y
    case _ => super.int_plus(x, y)
  }

  override def int_minus(x: Rep[Int], y: Rep[Int]) = (x, y) match {
    case (x, Const(0)) => x
    case _ => super.int_minus(x, y)
  }

  override def int_times(x: Rep[Int], y: Rep[Int]) = (x, y) match {
    case (x, Const(0)) => Const(0)
    case (Const(0), y) => Const(0)
    case (x, Const(1)) => x
    case (Const(1), y) => y
    case (x, Const(-1)) => int_minus(Const(0), x)
    case (Const(-1), y) => int_minus(Const(0), y)
    case _ => super.int_times(x, y)
  }
}
