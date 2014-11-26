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
import com.oracle.truffle.api.frame._;
import com.oracle.truffle.api.nodes._;
import com.oracle.truffle.api.nodes.Node._;

import scala.annotation.target.field

import org.scalatest._

import scala.collection.mutable.ArrayBuffer


trait Base {

  // truffle interface
  var runtime: TruffleRuntime = _
  var frameDescriptor: FrameDescriptor = _

  // base types

  trait Typ[T] {
    def slotKind: FrameSlotKind
  }

  type Rep[+T] = Exp[T]

  abstract class BaseNode extends Node with Product {
    def prettyString = productPrefix + "(" + ((0 until productArity) map productElement mkString ",") + ")"
    override def toString = prettyString
  }

  trait Exp[@specialized +T] extends BaseNode {
    def execute(frame: VirtualFrame): T
  }
  
  case class Sym[@specialized T:Typ](val slot: FrameSlot) extends Exp[T] {
    val kind = slot.getKind
    def execute(frame: VirtualFrame): T = {
      kind match {
        case FrameSlotKind.Int =>
          frame.getInt(slot).asInstanceOf[T]
        case FrameSlotKind.Boolean =>
          frame.getBoolean(slot).asInstanceOf[T]
        case FrameSlotKind.Long =>
          frame.getLong(slot).asInstanceOf[T]
        case FrameSlotKind.Double =>
          frame.getDouble(slot).asInstanceOf[T]
        case FrameSlotKind.Float =>
          frame.getFloat(slot).asInstanceOf[T]
        case FrameSlotKind.Byte =>
          frame.getByte(slot).asInstanceOf[T]
        case _ =>
          frame.getObject(slot).asInstanceOf[T]
      }
    }
  }

  case class Const[@specialized T:Typ](value: T) extends Exp[T] {
    def execute(frame: VirtualFrame): T = value
  }


  trait Def[@specialized T] extends BaseNode {
    def execute(frame: VirtualFrame): T
  }

  trait Stm extends BaseNode {
    def execute(frame: VirtualFrame): Unit
  }

  case class Read[@specialized T:Typ](value: Exp[T]) extends Def[T] {
    def execute(frame: VirtualFrame): T = value.execute(frame)
  }

  case class Assign[@specialized T:Typ](slot: FrameSlot, @(Child @field) d: Def[T]) extends Stm {
    val kind = slot.getKind
    def execute(frame: VirtualFrame): Unit = {
      kind match {
        case FrameSlotKind.Int =>
          frame.setInt(slot, d.execute(frame).asInstanceOf[Int])
        case FrameSlotKind.Boolean =>
          frame.setBoolean(slot, d.execute(frame).asInstanceOf[Boolean])
        case FrameSlotKind.Long =>
          frame.setLong(slot, d.execute(frame).asInstanceOf[Long])
        case FrameSlotKind.Double =>
          frame.setDouble(slot, d.execute(frame).asInstanceOf[Double])
        case FrameSlotKind.Float =>
          frame.setFloat(slot, d.execute(frame).asInstanceOf[Float])
        case FrameSlotKind.Byte =>
          frame.setByte(slot, d.execute(frame).asInstanceOf[Byte])
        case _ =>
          frame.setObject(slot, d.execute(frame))
      }
    }
  }

  case class Block[@specialized T:Typ](@(Children @field) stms: Array[Stm], res: Exp[T]) extends Node {
    @ExplodeLoop def execute(frame: VirtualFrame): T = {
      var i = 0
      while (i < stms.length) {
        stms(i).execute(frame)
        i += 1
      }
      //stms.foreach(_.execute(frame))
      res.execute(frame)
    }
    override def toString = stms.map(_.toString).mkString("\n")
  }

  // reflect & reify: create statements in evaluation order

  var localDefs: ArrayBuffer[Stm] = null

  var varCount = 0
  def fresh[T:Typ] = {
    varCount += 1
    Sym(frameDescriptor.addFrameSlot(s"x${varCount-1}", implicitly[Typ[T]].slotKind))
  }

  def createDefinition[T:Typ](v: Sym[T], d: Def[T]) = { localDefs += Assign(v.slot,d); v }

  def reflect[T:Typ](d: Def[T]): Exp[T] = createDefinition(fresh,d)

  def reify[T:Typ](d: => Exp[T]): Block[T] = {
    val save = localDefs
    localDefs = new ArrayBuffer
    val res = d
    val stms = localDefs.toArray
    localDefs = save
    Block(stms,res)
  }

  // cross-stage persistance: implicit lifting of constants

  implicit def lift[T:Typ](x: T): Exp[T] = Const(x)


  // access to arguments of Truffle CallTarget

  case class GetArg[@specialized T:Typ](index: Int) extends Def[T] {
    def execute(frame: VirtualFrame) = {
      val args = frame.getArguments()(0).asInstanceOf[Array[Object]];
      args(index).asInstanceOf[T]
    }
  }

  def getArg[T:Typ](index: Int): Exp[T] = reflect(GetArg[T](index))

  // mutable variables

  case class Var[T:Typ](v:Sym[T]) {
    def apply(): Rep[T] = reflect(Read(v))
    def update(y:Rep[T]) = {
      // TODO: avoid low-level access
      localDefs += Assign(v.slot,Read(y)); v
    }
  }
  def cell[T:Typ](x: Rep[T]) = {
    Var(createDefinition(fresh,Read(x)))
  }



  // (optional) root node handling

  class LMSRootNode[@specialized T](desc: FrameDescriptor, @(Child @field) val block: Block[T]) extends RootNode(null, desc) {
    override def execute(frame: VirtualFrame): AnyRef = block.execute(frame).asInstanceOf[AnyRef]
  }

  def lms[T:Typ,U:Typ](f: Rep[T] => Rep[U]) = new (T=>U) {
    val rootNode = {
      val saveC = varCount
      val saveD = frameDescriptor
      try {
        varCount = 0
        frameDescriptor = new FrameDescriptor();
        val t = reify(f(getArg[T](0)));
        new LMSRootNode(frameDescriptor,t);
      } finally {
        //varCount = saveC
        //frameDescriptor = saveD
      }
    }
    val target = runtime.createCallTarget(rootNode)

    override def apply(x: T) = {
      val result = target.call(Array(x.asInstanceOf[AnyRef]));
      result.asInstanceOf[U]
    }
  }
  
  def lms[T1:Typ,T2:Typ,U:Typ](f: (Rep[T1],Rep[T2]) => Rep[U]) = new ((T1,T2)=>U) {
    val rootNode = {
      val saveC = varCount
      val saveD = frameDescriptor
      try {
        varCount = 0
        frameDescriptor = new FrameDescriptor();
        new LMSRootNode(frameDescriptor,reify(f(getArg[T1](0),getArg[T2](1))));
      } finally {
        //varCount = saveC
        //frameDescriptor = saveD
      }
    }
    val target = runtime.createCallTarget(rootNode)

    override def apply(x1: T1, x2: T2) = {
      val result = target.call(Array(x1.asInstanceOf[AnyRef],x2.asInstanceOf[AnyRef]));
      result.asInstanceOf[U]
    }
  }


   class LMSNestedRootNode[@specialized T](@(Child @field) val block: Block[T]) extends RootNode {
    override def execute(frame: VirtualFrame): AnyRef = {
      val parentFrame = frame.getArguments()(0);
      block.execute(parentFrame.asInstanceOf[VirtualFrame]).asInstanceOf[AnyRef]
    }
    override def toString = s"LMSNestedRootNode($block)"
  }

  case class LMSNestedCallNode[@specialized T](@(Child @field) val target: CallTarget, rootNode: LMSNestedRootNode[T]) extends Def[T] {
    override def execute(frame: VirtualFrame): T = {
      if (CompilerDirectives.inInterpreter)
        target.call(frame).asInstanceOf[T]
      else
        rootNode.block.execute(frame).asInstanceOf[T]
    }
    override def toString = s"LMSNestedCallNode($target)"
  }


  def fun[@specialized U:Typ](f: () => Rep[U]) = new (()=>Rep[U]) {
    val rootNode = new LMSNestedRootNode(reify(f()))
    val target = runtime.createCallTarget(rootNode)

    override def apply() = {
      reflect(LMSNestedCallNode(target,rootNode))
    }
  }
}

trait Primitives extends Types with IntegerType with DoubleType with ArrayType
trait TruffleLMS extends Base with Primitives with ControlFlow
