package lms
import sql.Scanner

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import scala.annotation.target.field
import org.scalatest._

trait ScannerType extends Base with Types {

  case class ScannerNew(@(Child @field) filename: Rep[String]) extends Def[Scanner] {
    def execute(frame: VirtualFrame) = {
      val descr = frame.getFrameDescriptor(); 
      val s = new Scanner(filename.execute(frame))
      s
    }
  }

  case class ScannerNext(@(Child @field) s: Rep[Scanner], d: Char) extends Def[String] {
    def execute(frame: VirtualFrame) = {
      val str = s.execute(frame).next(d);
      str
    }
  }

  case class ScannerHasNext(@(Child @field) s: Rep[Scanner]) extends Def[Boolean] {
    def execute(frame: VirtualFrame) = {
      s.execute(frame).hasNext;
    }
  }

  case class ScannerClose(@(Child @field) s: Rep[Scanner]) extends Def[Unit] {
    def execute(frame: VirtualFrame) = {
      s.execute(frame).close;
    }
  }

  implicit class RepScannerOps(s: Rep[Scanner]) {
    def next(d: Char) = scannerNext(s, d)
    def hasNext = scannerHasNext(s)
    def close = scannerClose(s)
  }

  def newScanner(fn: Rep[String]): Rep[Scanner] = reflect(ScannerNew(fn))
  def scannerNext(s: Rep[Scanner], d: Char): Rep[String] = reflect(ScannerNext(s, d))
  def scannerHasNext(s: Rep[Scanner]): Rep[Boolean] = reflect(ScannerHasNext(s))
  def scannerClose(s: Rep[Scanner]): Rep[Unit] = reflect(ScannerClose(s))
}

