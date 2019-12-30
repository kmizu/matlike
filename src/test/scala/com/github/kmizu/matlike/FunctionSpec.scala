package com.github.kmizu.matlike

class FunctionSpec extends SpecHelper {
  describe("substring") {
    val expectations: List[(String, Value)] = List(
      """
        |substring("FOO", 0, 1)
      """.stripMargin -> ObjectValue("F"),
      """
        |substring("FOO", 0, 2)
      """.stripMargin -> ObjectValue("FO"),
      """
        |substring("FOO", 0, 3)
      """.stripMargin -> ObjectValue("FOO"),
      """
        |substring("FOO", 1, 1)
      """.stripMargin -> ObjectValue(""),
      """
        |substring("FOO", 1, 2)
      """.stripMargin -> ObjectValue("O"),
      """
        |substring("FOO", 1, 3)
      """.stripMargin -> ObjectValue("OO")
    )

    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
  describe("at") {
    val expectations: List[(String, Value)] = List(
      """
        |at("FOO", 0)
      """.stripMargin -> ObjectValue("F"),
      """
        |at("FOO", 1)
      """.stripMargin -> ObjectValue("O"),
      """
        |at("FOO", 2)
      """.stripMargin -> ObjectValue("O")
    )

    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
  describe("matches") {
    val expectations: List[(String, Value)] = List(
      """
        |matches("FOO", ".*")
      """.stripMargin -> BoxedBoolean(true),
      """
        |matches("FOO", "FOO")
      """.stripMargin -> BoxedBoolean(true),
      """
        |matches("FOO", "FO")
      """.stripMargin -> BoxedBoolean(false),
      """
        |matches("FO", "FOO")
      """.stripMargin -> BoxedBoolean(false)
    )

    expectations.foreach { case (in, expected) =>
      it(s"${in} evaluates to ${expected}") {
        assert(expected == E(in))
      }
    }
  }
}
