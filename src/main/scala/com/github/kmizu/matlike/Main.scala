package com.github.kmizu.matlike

import java.io.File

import com.github.scaruby.SFile

import scala.collection.Iterator.continually
import scala.util.boundary

/**
 * @author Kota Mizushima
 */
object Main {
  class REPL(val evaluator: Evaluator) {
    private def isInputComplete(input: String): Boolean = {
      boundary:
        val stack = new scala.collection.mutable.Stack[Char]()
        var inString = false
        
        input.foreach { c =>
          if (inString) {
            if (c == '"') inString = false
          } else {
            c match {
              case '"' => inString = true
              case '(' | '[' | '{' => stack.push(c)
              case ')' => if (stack.isEmpty || stack.pop() != '(') boundary.break(true) else ()
              case ']' => if (stack.isEmpty || stack.pop() != '[') boundary.break(true) else ()
              case '}' => if (stack.isEmpty || stack.pop() != '{') boundary.break(true) else ()
              case _ => ()
            }
          }
        }
        
        stack.isEmpty && !inString && !input.matches(""".*[\+\-*/=]>?$""")
    }

    def start(): Unit = {
      var nextLineIsRequested = true
      var currentBuffer = new StringBuilder
      
      while(nextLineIsRequested) {
        val prompt = if (currentBuffer.isEmpty) "> " else "... "
        Console.print(prompt)
        Console.flush()
        
        val line = Console.in.readLine()
        Console.flush()
        
        if (line == null) { // Handle Ctrl+D
          nextLineIsRequested = false
        } else if (line.stripLineEnd == ":exit") {
          nextLineIsRequested = false
        } else if (line.stripLineEnd == ":reset") {
          currentBuffer.clear()
          println("Input reset")
        } else {
          currentBuffer.append(line).append("\n")
          if (isInputComplete(currentBuffer.toString)) {
            try {
              val value = evaluator.evaluateString(currentBuffer.toString)
              println(s"value = ${value}")
            } catch {
              case e: Exception =>
                println(s"Error: ${e.getMessage}")
            }
            currentBuffer.clear()
          }
        }
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val evaluator = new Evaluator
    parseCommandLine(args) match {
      case Some(("-e", line)) =>
        println(evaluator.evaluateString(line))
      case Some(("-f", fileName)) =>
        evaluator.evaluateFile(new SFile(fileName))
      case None =>
        new REPL(evaluator).start()
      case _ =>
        Console.err.println(
          """
            |Usage: java -jar matlike.jar (-f <fileName> | -e <expression>)
            |<fileName>   : read a program from <fileName> and execute it
            |-e <expression> : evaluate <expression>
          """.stripMargin)
    }
  }

  def parseCommandLine(args: Array[String]): Option[(String, String)] = {
    val paser = new Parser
    args match {
      case Array(fileName) if fileName.endsWith("mtlk") =>
        Some("-f"-> fileName)
      case Array("-e", line) =>
        Some("-e" -> line)
      case otherwise =>
        None
    }
  }
}
