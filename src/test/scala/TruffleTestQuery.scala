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

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import org.scalatest._
import query.SQLParser
import query.QueryAST
import query.QueryProcessor
import query.PlainQueryProcessor
import scala.annotation.meta.field
import query.Scanner

class TruffleTestQuery extends FunSuite with QueryBase {
  //   case class Scan(name: Table, schema: Schema, delim: Char, extSchema: Boolean) extends Operator
  //  case class PrintCSV(parent: Operator) extends Operator
  //  case class Project(outSchema: Schema, inSchema: Schema, parent: Operator) extends Operator
  //  case class Filter(pred: Predicate, parent: Operator) extends Operator
  //  case class Join(parent1: Operator, parent2: Operator) extends Operator
  //  case class Group(keys: Schema, agg: Schema, parent: Operator) extends Operator
  //  case class HashJoin(parent1: Operator, parent2: Operator) extends Operator
  //  sealed abstract class Predicate
  //  case class Eq(a: Ref, b: Ref) extends Predicate
  //
  //  sealed abstract class Ref
  //  case class Field(name: String) extends Ref
  //  case class Value(x: Any) extends Ref

  case class ScanNode(filename: String, schema: Schema, fieldDelimiter: Char, externalSchema: Boolean)(yld: Record => Unit) extends BaseNode {
    def execute(f: Frame): Vector[Record] = {
      val s = new Scanner(filename)
      val last = schema.last
      var r = Vector[Record]()
      def nextRecord = Record(schema.map { x => s.next(if (x == last) '\n' else fieldDelimiter) }, schema)
      if (!externalSchema) {
        r :+ nextRecord
      }
      while (s.hasNext) r :+ nextRecord
      s.close
      r
    }
  }

  class TRootNode[@specialized T](desc: FrameDescriptor, @(Child @field) val block: ScanNode) extends RootNode(null, desc) {
    override def execute(frame: VirtualFrame): AnyRef = block.execute(frame).asInstanceOf[AnyRef]
  }

  test("qTest") {
    val runtime: TruffleRuntime = Truffle.getRuntime()
    val descriptor: FrameDescriptor = new FrameDescriptor()
    val schema = Vector[String]("Name", "Value", "Flag")
    val delim: Char = ','
    val rootNode = new TRootNode(descriptor, new ScanNode("src/data/t.csv", schema, delim, false)({ _ => }))
    val target: CallTarget = runtime.createCallTarget(rootNode);
    val result = target.call();
    println(result)

  }

}

