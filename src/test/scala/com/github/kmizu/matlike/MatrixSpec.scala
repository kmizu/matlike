package com.github.kmizu.matlike

import com.github.kmizu.matlike.Type.{TConstructor, TInt, TLiteral}

class MatrixSpec extends SpecHelper {
  describe("literal") {
    val expectations = List[(String, Value)](
      """["a"]""" -> matrixOf(rowOf("a")),
      "[1, 2]" -> matrixOf(rowOf(1, 2)),
      """|[1
        | 2]
      """.stripMargin -> (matrixOf(rowOf(1), rowOf(2))),
      """|[1
        |
        | 2]
      """.stripMargin -> (matrixOf(rowOf(1) ,rowOf(2))),
      """|[1 +
        |
        | 2]
      """.stripMargin -> matrixOf(rowOf(3)),
      """|[1 2
         | 3 4]
      """.stripMargin -> matrixOf(rowOf(1, 2), rowOf(3, 4)),
      """| [[1 2]
         |  [3 4]]
      """.stripMargin ->
        matrixOf(
          rowOf(matrixOf(rowOf(1, 2))),
          rowOf(matrixOf(rowOf(3, 4)))
        )
      )
    expectations.zipWithIndex.foreach { case ((in, expected), i) =>
      it(s"the evaluations are done expectedly (${i})") {
        assert(expected == E(in))
      }
    }
  }
  describe("type") {
    it(s"infers the type with type annotation") {
      assert(
        TConstructor("Matrix", List(TInt, TLiteral(2), TLiteral(2))) ==
        typeOf(
          """
            | val x: Matrix<Int, 2, 2> = [
            |   1 2
            |   3 4
            | ]
            | x
            |""".stripMargin
        )
      )
    }
    it(s"infers the type without type annotation") {
      assert(
        TConstructor("Matrix", List(TInt, TLiteral(2), TLiteral(3))) ==
          typeOf(
            """
              | val x = [
              |   1 2 3
              |   4 5 6
              | ]
              | x
              |""".stripMargin
          )
      )
    }
  }
  describe("addition") {
    it("infers the result type") {
      val input =
        """
          |val m1 = [
          |  1 2 3
          |  4 5 6
          |]
          |val m2 = [
          |  2 3 4
          |  5 6 7
          |]
          |m1 + m2
          |""".stripMargin
      assert(
        TConstructor("Matrix", List(TInt, TLiteral(2), TLiteral(3))) == typeOf(input)
      )
    }
    it("computes the result") {
      val input =
        """
          |val m1 = [
          |  1 2 3
          |  4 5 6
          |]
          |val m2 = [
          |  2 3 4
          |  5 6 7
          |]
          |m1 + m2
          |""".stripMargin
      assert(
        matrixOf(
          rowOf(3, 5, 7),
          rowOf(9, 11, 13)
        ) == E(input)
      )
    }
  }
  describe("addition with scalar") {
    it("infers the result type") {
      val input =
        """
          |val m = [
          |  1 2
          |  3 4
          |]
          |m + 3
          |""".stripMargin
      assert(
        TConstructor("Matrix", List(TInt, TLiteral(2), TLiteral(2))) == typeOf(input)
      )
    }
    it("computes the result") {
      val input =
        """
          |val m = [
          |  1 2
          |  3 4
          |]
          |m + 3
          |""".stripMargin
      assert(
        matrixOf(
          rowOf(4, 5),
          rowOf(6, 7)
        ) == E(input)
      )
    }
  }
  describe("subtraction") {
    it("infers the result type") {
      val input =
        """
          |val m1 = [
          |  1 2 3
          |  4 5 6
          |]
          |val m2 = [
          |  2 3 4
          |  5 6 7
          |]
          |m1 - m2
          |""".stripMargin
      assert(
        TConstructor("Matrix", List(TInt, TLiteral(2), TLiteral(3))) == typeOf(input)
      )
    }
    it("computes the result") {
      val input =
        """
          |val m1 = [
          |  1 2 3
          |  4 5 6
          |]
          |val m2 = [
          |  2 3 4
          |  5 6 7
          |]
          |m1 - m2
          |""".stripMargin
      assert(
        matrixOf(
          rowOf(-1, -1, -1),
          rowOf(-1, -1, -1)
        ) == E(input)
      )
    }
  }
  describe("subtraction with scalar") {
    it("infers the result type") {
      val input =
        """
          |val m = [
          |  1 2 3
          |  4 5 6
          |]
          |m - 2
          |""".stripMargin
      assert(
        TConstructor("Matrix", List(TInt, TLiteral(2), TLiteral(3))) == typeOf(input)
      )
    }
    it("computes the result") {
      val input =
        """
          |val m = [
          |  1 2 3
          |  4 5 6
          |]
          |m - 2
          |""".stripMargin
      assert(
        matrixOf(
          rowOf(-1, 0, 1),
          rowOf(2, 3, 4)
        ) == E(input)
      )
    }
  }
  describe("product") {
    it("infers the result type") {
      val input =
        """
          |def mult(x, y) = x * y
          |mult(
          |  [1 2 3
          |   4 5 6],
          |  [2
          |   4
          |   6]
          |)
          |""".stripMargin
      assert(
        TConstructor("Matrix", List(TInt, TLiteral(2), TLiteral(1))) ==
        typeOf(input)
      )
    }
    it("computes the result") {
      val input =
        """
          |val mA = [
          |  1 2 3
          |  1 0 1
          |]
          |val mB = [
          |  2
          |  3
          |  1
          |]
          |mA _*_ mB
          |""".stripMargin
      assert(
        matrixOf(
          rowOf(11),
          rowOf(3)
        ) ==
        E(input)
      )
    }
  }
  describe("product with scalar") {
    it("infers the result type - matrix * scalar") {
      val input =
        """
          |def mult(x, y) = x * y
          |val m =  [
          |  1 2 3
          |  4 5 6
          |]
          |m * 3
          |""".stripMargin
      assert(
        TConstructor("Matrix", List(TInt, TLiteral(2), TLiteral(3))) == typeOf(input)
      )
    }
    it("computes the result - matrix * scalar") {
      val input =
        """
          |val m = [
          |  1 2 3
          |  4 5 6
          |]
          |m * 3
          |""".stripMargin
      assert(
        matrixOf(
          rowOf(3, 6, 9),
          rowOf(12, 15, 18)
        ) == E(input)
      )
    }
    it("infers the result type - scalar * matrix") {
      val input =
        """
          |val m =  [
          |  1 2 3
          |  4 5 6
          |]
          |4 * m
          |""".stripMargin
      assert(
        TConstructor("Matrix", List(TInt, TLiteral(2), TLiteral(3))) == typeOf(input)
      )
    }
    it("computes the result - scalar * matrix") {
      val input =
        """
          |val m = [
          |  1 2 3
          |  4 5 6
          |]
          |4 * m
          |""".stripMargin
      assert(
        matrixOf(
          rowOf(4, 8, 12),
          rowOf(16, 20, 24)
        ) == E(input)
      )
    }
  }
  describe("division") {
    it("infers the result type") {
      val input =
        """
          |val m1 = [
          |  1 2 3
          |  4 5 6
          |]
          |val m2 = [
          |  1 2 3
          |  3 3 2
          |]
          |m1 / m2
          |""".stripMargin
      assert(
        TConstructor("Matrix", List(TInt, TLiteral(2), TLiteral(3))) == typeOf(input)
      )
    }
    it("computes the result") {
      val input =
        """
          |val m1 = [
          |  1 2 3
          |  4 5 6
          |]
          |val m2 = [
          |  1 2 3
          |  3 3 2
          |]
          |m1 / m2
          |""".stripMargin
      assert(
        matrixOf(
          rowOf(1, 1, 1),
          rowOf(1, 1, 3)
        ) == E(input)
      )
    }
  }
  describe("division with scalar") {
    it("infers the result type - matrix / scalar") {
      val input =
        """
          |val m = [
          |  1 2 3
          |  4 5 6
          |]
          |m / 2
          |""".stripMargin
      assert(
        TConstructor("Matrix", List(TInt, TLiteral(2), TLiteral(3))) == typeOf(input)
      )
    }
    it("computes the result - matrix / scalar") {
      val input =
        """
          |val m = [
          |  1 2 3
          |  4 5 6
          |]
          |m / 2
          |""".stripMargin
      assert(
        matrixOf(
          rowOf(0, 1, 1),
          rowOf(2, 2, 3)
        ) == E(input)
      )
    }
  }
}
