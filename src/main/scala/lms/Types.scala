package lms
import sql.Scanner

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import org.scalatest._

trait Types extends Base {

  implicit object unitTyp extends Typ[Unit]{
    def slotKind = FrameSlotKind.Object
  }
  
  implicit object intTyp extends Typ[Int] {
    def slotKind = FrameSlotKind.Int
  }
  implicit object boolTyp extends Typ[Boolean] {
    def slotKind = FrameSlotKind.Boolean
  }

  implicit object doubleTyp extends Typ[Double] {
    def slotKind = FrameSlotKind.Double
  }
  
  implicit object stringTyp extends Typ[String] {
    def slotKind = FrameSlotKind.Object
  }
  
  implicit def arrayTyp[T:Typ] = new Typ[Array[T]] {
    def slotKind = FrameSlotKind.Object
  }
  
  implicit def scannerTyp = new Typ[Scanner] {
    def slotKind = FrameSlotKind.Object
  }

}
