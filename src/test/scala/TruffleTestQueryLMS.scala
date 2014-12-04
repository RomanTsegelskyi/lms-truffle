///*
// * Copyright (c) 2014, Oracle and/or its affiliates. All rights reserved.
// * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
// *
// * This code is free software; you can redistribute it and/or modify it
// * under the terms of the GNU General Public License version 2 only, as
// * published by the Free Software Foundation.
// *
// * This code is distributed in the hope that it will be useful, but WITHOUT
// * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// * version 2 for more details (a copy is included in the LICENSE file that
// * accompanied this code).
// *
// * You should have received a copy of the GNU General Public License version
// * 2 along with this work; if not, write to the Free Software Foundation,
// * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
// *
// * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
// * or visit www.oracle.com if you need additional information or have any
// * questions.
// */
//
//
//
//import com.oracle.truffle.api._
//import com.oracle.truffle.api.frame._
//import com.oracle.truffle.api.nodes._
//import com.oracle.truffle.api.nodes.Node._
//import org.scalatest._
//import scala.annotation.meta.field
//import java.io.PrintStream
//import java.io.ByteArrayOutputStream
//import SQL.QueryBaseLMS
//
//class TruffleTestQueryLMS extends FunSuite with QueryBaseLMS {
//
//  class TRootNode[@specialized T](desc: FrameDescriptor, @(Child @field) val block: OperatorNode) extends RootNode(null, desc) {
//    def execute(frame: VirtualFrame): AnyRef = block.execute(frame) { _ => }.asInstanceOf[AnyRef]
//  }
//
//  test("PrintTest") {
//    val runtime: TruffleRuntime = Truffle.getRuntime()
//    val descriptor: FrameDescriptor = new FrameDescriptor()
//    val schema = Vector[String]("Name", "Value", "Flag")
//    val delim: Char = ','
//    val rootNode = new TRootNode(descriptor, new PrintCSVNode(new ScanNode("src/data/t.csv", schema, delim, false)))
//    val target: CallTarget = runtime.createCallTarget(rootNode);
//    val output = new ByteArrayOutputStream()
//    scala.Console.setOut(new PrintStream(output))
//    Console.withOut(new PrintStream(output))(target.call());
//    assert(output.toString === 
//"""Name,Value,Flag
//A,7,no
//B,2,yes
//""")
//  }
//
//  test("ProjectTest") {
//    val runtime: TruffleRuntime = Truffle.getRuntime()
//    val descriptor: FrameDescriptor = new FrameDescriptor()
//    val schema = Vector[String]("Name", "Value", "Flag")
//    val outSchema = Vector[String]("Name")
//    val delim: Char = ','
//    val rootNode = new TRootNode(descriptor, new PrintCSVNode(
//    new ProjectNode(outSchema, outSchema, new ScanNode("src/data/t.csv", schema, delim, false))))
//    val target: CallTarget = runtime.createCallTarget(rootNode);
//    val output = new ByteArrayOutputStream()
//    scala.Console.setOut(new PrintStream(output))
//    Console.withOut(new PrintStream(output))(target.call());
//    assert(output.toString === 
//"""Name
//A
//B
//""")
//  }
//  
//  test("FilterTest") {
//    val runtime: TruffleRuntime = Truffle.getRuntime()
//    val descriptor: FrameDescriptor = new FrameDescriptor()
//    val schema = Vector[String]("Name", "Value", "Flag")
//    val outSchema = Vector[String]("Name")
//    val delim: Char = ','
//    val query = new PrintCSVNode(
//      new ProjectNode(outSchema, outSchema, 
//          new FilterNode(Eq(Field("Flag"), Value("yes")), new ScanNode("src/data/t.csv", schema, delim, false))))
//    val rootNode = new TRootNode(descriptor, query)
//    val target: CallTarget = runtime.createCallTarget(rootNode);
//    val output = new ByteArrayOutputStream()
//    scala.Console.setOut(new PrintStream(output))
//    Console.withOut(new PrintStream(output))(target.call());
//    assert(output.toString === 
//"""Name
//B
//""")
//    
//  }
//
//}
//
