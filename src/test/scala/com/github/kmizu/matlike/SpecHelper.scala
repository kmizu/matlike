package com.github.kmizu.matlike

import org.scalatest.{DiagrammedAssertions, FunSpec}
import java.util.ArrayList
import java.util.List
import java.util.HashMap

trait SpecHelper extends FunSpec with DiagrammedAssertions {
  val E = new Evaluator

  def matrixOf(rows: List[Any]*): MatrixValue = {
    val newMatrix = new ArrayList[List[Any]]
    rows.foreach{row => newMatrix.add(row)}
    MatrixValue(newMatrix)
  }

  def rowOf(elements: Any*): ArrayList[Any] = {
    val newList = new ArrayList[Any]
    elements.foreach { e =>
      newList.add(e)
    }
    newList
  }
  def mapOf[K, V](kvs: (K, V)*): HashMap[K, V] = {
    val newMap = new HashMap[K, V]
    for((k, v) <- kvs) {
      newMap.put(k, v)
    }
    newMap
  }
  def expect[A, B](label: String)(expectation: (String, Value)): Unit = expectation match {
    case (actual, expected) =>
      it(label) {
        assertResult(expected)(E.evaluateString(actual))
      }
  }
}
