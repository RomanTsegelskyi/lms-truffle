package lms

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import scala.annotation.target.field
import org.scalatest._
import math._

// Why can't extend base directly 
trait Trig extends Primitives {

  case class CosNode(@(Child @field) x: Exp[Double]) extends Def[Double] {
    def execute(frame: VirtualFrame) = {
      math.cos(x.execute(frame))
    }
  }

  case class SinNode(@(Child @field) x: Exp[Double]) extends Def[Double] {
    def execute(frame: VirtualFrame) = {
      math.sin(x.execute(frame))
    }
  }
  
  def cos(x:Rep[Double]) = reflect(CosNode(x))
  def sin(x:Rep[Double]) = reflect(SinNode(x))
}

trait TrigOpt extends Trig{
  
  override def sin(x: Rep[Double]) = x match {
    case Const(f) => lift(math.sin(f))
    case _ => super.sin(x)
  }
  
  override def cos(x: Rep[Double]) =  x match {
    case Const(f) => lift(math.cos(f))
    case _ => super.cos(x)
  }
}


trait TrigOptFFT extends TrigOpt {
  val sin_values = Map(-2*Pi -> 0.0, -3.0/2*Pi -> 1.0, -Pi -> 0.0, -1.0/2*Pi -> -1.0, 0.0 -> 0.0, 1.0/2*Pi -> 1.0, Pi -> 0.0, 3.0/2*Pi -> -1.0, 2*Pi -> 0.0)
  val cos_values = Map(-2*Pi -> 1.0, -3.0/2*Pi -> 0.0, -Pi -> -1.0, -1.0/2*Pi -> 0.0, 0.0 -> 1.0, 1.0/2*Pi -> 0.0, Pi -> -1.0, 3.0/2*Pi -> 0.0, 2*Pi -> 1.0)

  override def sin(x: Rep[Double]) = x match {
    case Const(f) => if (sin_values.contains(f)) Const(sin_values(f).asInstanceOf[Double]) else lift(math.sin(f))
    case _ => super.sin(x)
  }
  
  override def cos(x: Rep[Double]) =  x match {
    case Const(f) => if (cos_values.contains(f)) Const(cos_values(f).asInstanceOf[Double]) else lift(math.cos(f))
    case _ => super.cos(x)
  }
}