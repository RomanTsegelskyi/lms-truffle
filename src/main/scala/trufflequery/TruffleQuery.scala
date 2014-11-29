import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import scala.annotation.target.field
import org.scalatest._
import scala.collection.mutable.ArrayBuffer
import query.Scanner

trait QueryBase {

  // base types

  abstract class BaseNode extends Node with Product {
    def prettyString = productPrefix + "(" + ((0 until productArity) map productElement mkString ",") + ")"
    override def toString = prettyString
  }

  type Table
  type Schema = Vector[String]
  type Fields = Vector[String]

  case class Record(fields: Fields, schema: Schema) {
    def apply(key: String): String = fields(schema indexOf key)
    def apply(keys: Schema): Fields = keys.map(this apply _)
  }

  // relational algebra ops
  abstract class OperatorNode extends BaseNode
  case class PrintCSVNode(parent: OperatorNode) extends OperatorNode
  case class ProjectNode(outSchema: Schema, inSchema: Schema, parent: OperatorNode) extends OperatorNode
  case class FilterNode(pred: Predicate, parent: OperatorNode) extends OperatorNode
  case class JoinNode(parent1: OperatorNode, parent2: OperatorNode) extends OperatorNode
  case class GroupNode(keys: Schema, agg: Schema, parent: OperatorNode) extends OperatorNode
  case class HashJoinNode(parent1: OperatorNode, parent2: OperatorNode) extends OperatorNode

  // filter predicates
  sealed abstract class Predicate
  case class EqNode(a: Ref, b: Ref) extends Predicate

  sealed abstract class Ref
  case class Field(name: String) extends Ref
  case class Value(x: Any) extends Ref

  
}
