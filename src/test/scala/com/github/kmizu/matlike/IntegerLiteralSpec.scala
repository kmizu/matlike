package com.github.kmizu.matlike

import java.util.ArrayList

class IntegerLiteralSpec extends SpecHelper {
  describe("literals") {
    val expectations = List[(String, Value)](
      "2" -> BoxedInt(2),
      "+2" -> BoxedInt(+2),
      "-2" -> BoxedInt(-2),
      "1" -> BoxedInt(1),
      "+1" -> BoxedInt(+1),
      "-1" -> BoxedInt(-1),
      "0" -> BoxedInt(0),
      "+0" -> BoxedInt(0),
      "-0" -> BoxedInt(0),
      s"${Int.MinValue}" -> BoxedInt(Int.MinValue),
      s"-${Int.MinValue}" -> BoxedInt(-Int.MinValue),
      s"${Int.MaxValue}" -> BoxedInt(Int.MaxValue),
      s"-${Int.MaxValue}" -> BoxedInt(-Int.MaxValue)
    )
    expectations.zipWithIndex.foreach { case ((in, expected), i) =>
      it(s"the literal is evaluated expectedly (${i})") {
        assert(expected == E(in))
      }
    }
  }
}
