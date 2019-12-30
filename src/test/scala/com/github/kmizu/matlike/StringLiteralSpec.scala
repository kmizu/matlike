package com.github.kmizu.matlike

class StringLiteralSpec extends SpecHelper {
  describe("literal with escape sequence") {
    val expectations = List[(String, Value)](
      """"\r\n"""" -> ObjectValue("\r\n"),
      """"\r"""" -> ObjectValue("\r"),
      """"\n"""" -> ObjectValue("\n"),
      """"\t"""" -> ObjectValue("\t"),
      """"\b"""" -> ObjectValue("\b"),
      """"\f"""" -> ObjectValue("\f"),
      """"\\"""" -> ObjectValue("\\")
    )
    expectations.zipWithIndex.foreach{ case ((in, expected), i) =>
      it(s"the literal is evaluated expectedly (${i})") {
        assert(expected == E(in))
      }
    }
  }
}
