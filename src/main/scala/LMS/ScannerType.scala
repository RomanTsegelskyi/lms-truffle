/*
 * Copyright (c) 2014, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package LMS
import SQL.Scanner

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
      val slot = descr.addFrameSlot("tscanner", FrameSlotKind.Object);
      val s = new Scanner(filename.execute(frame))
      frame.setObject(slot, s);
      s
    }
  }

  case class ScannerNext(@(Child @field) s: Rep[Scanner], d: Char) extends Def[String] {
    def execute(frame: VirtualFrame) = {
      val str = s.execute(frame).next(d);
      str
    }
  }

  case class ScannerHasNext(@(Child @field) s: Rep[Scanner]) extends Rep[Boolean] {
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
  def scannerHasNext(s: Rep[Scanner]): Rep[Boolean] = ScannerHasNext(s)
  def scannerClose(s: Rep[Scanner]): Rep[Unit] = reflect(ScannerClose(s))
}

