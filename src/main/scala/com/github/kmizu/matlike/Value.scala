package com.github.kmizu.matlike

import com.github.kmizu.matlike.TypedAst.TypedNode
import scala.jdk.CollectionConverters._
import java.util

sealed abstract class Value
case class BoxedInt(value: Int) extends Value {
  override def toString = value.toString
}
case class BoxedBoolean(value: Boolean) extends Value {
  override def toString = value.toString
}
case class FunctionValue(value: TypedAst.FunctionLiteral, cleanup: Option[TypedNode], environment: Option[RuntimeEnvironment]) extends Value {
  override def toString = s"<function value>"
}
case class NativeFunctionValue(body: PartialFunction[List[Value], Value]) extends Value {
  override def toString = s"<native function>"
}
case object UnitValue extends Value {
  override def toString = "()"
}
case class ObjectValue(value: AnyRef) extends Value {
  override def toString = if(value eq null) "null" else value.toString
}
case class MatrixValue(rows: util.List[util.List[Any]]) extends Value {
  lazy val rowLength: Int = rows.size
  lazy val colLength: Int = rows.get(0).size
  def foreachElement[A](block: Any => A): Unit = {
    var i = 0
    while(i < rowLength) {
      var j = 0
      while(j < colLength) {
        block(rows.get(i).get(j))
        j += 1
      }
      i += 1
    }
  }
  def rowsIterable: Iterable[Iterable[Any]] = {
    rows.asScala.map(_.asScala.toIterable).toIterable
  }

  override def toString: String = {
    val builder = new StringBuilder
    val separator = System.getProperty("line.separator")
    builder.append(separator)
    builder.append("[" + separator)
    rows.asScala.foreach{row =>
      builder.append("  " + row.asScala.mkString(" "))
      builder.append(separator)
    }
    builder.append("]" + separator)
    builder.toString()
  }
}
object Value {

  def classOfValue(value: Value): java.lang.Class[_]= value match {
    case BoxedBoolean(v) => classOf[Boolean]
    case BoxedInt(v) => classOf[Int]
    case ObjectValue(v) => v.getClass
    case otherwise => otherwise.getClass
  }

  def boxedClassOfValue(value: Value): java.lang.Class[_]= value match {
    case BoxedBoolean(v) => classOf[java.lang.Boolean]
    case BoxedInt(v) => classOf[java.lang.Integer]
    case ObjectValue(v) => v.getClass
    case otherwise => otherwise.getClass
  }

  def boxedClassesOfValues(values: Array[Value]): Array[java.lang.Class[_]] = values.map(boxedClassOfValue)

  def classesOfValues(values: Array[Value]):  Array[java.lang.Class[_]] = values.map(classOfValue)

  def fromKlassic(value: Value): AnyRef = value match {
    case BoxedBoolean(v) => java.lang.Boolean.valueOf(v)
    case BoxedInt(v) => java.lang.Integer.valueOf(v)
    case ObjectValue(v) => v
    case UnitValue => UnitValue
    case otherwise => otherwise
  }

  def toKlassic(value: AnyRef): Value = value match {
    case v:java.lang.Boolean => BoxedBoolean(v.booleanValue())
    case v:java.lang.Integer => BoxedInt(v.intValue())
    case UnitValue => UnitValue
    case otherwise => ObjectValue(otherwise)
  }
}
