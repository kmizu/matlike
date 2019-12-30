package com.github.kmizu.matlike

import scala.util.matching.Regex
import com.github.kmizu.scomb
import com.github.kmizu.scomb.{Result, SCombinator}
import com.github.kmizu.matlike.Ast._
import com.github.kmizu.matlike.Type._

import scala.collection.mutable

/**
 * @author Kota Mizushima
 */
class Parser extends Processor[String, Program] {
  object Core extends SCombinator {
    def publicLocations: mutable.Map[Int, scomb.Location] = locations

    implicit def stringToParser(literal: String): Parser[String] = $(literal)

    implicit def regexToParser(literal: Regex): Parser[String] = regularExpression(literal)

    def %% : Parser[SourceLocation] = % ^^ { l =>
      SourceLocation(l.line, l.column)
    }

    def commit[T](parser: Parser[T]): Parser[T] = parser.commit

    lazy val LINEFEED: Parser[String] = ("\r\n" | "\r" | "\n")

    lazy val SEMICOLON: Parser[String] = ";"

    lazy val ANY: Parser[String] = any ^^ {
      _.toString
    }

    lazy val SPACING: Parser[String] = rule {
      (COMMENT | "\r\n" | "\r" | "\n" | " " | "\t" | "\b" | "\f").* ^^ {
        _.mkString
      }
    }

    lazy val SPACING_WITHOUT_LF: Parser[String] = rule {
      (COMMENT | "\t" | " " | "\b" | "\f").* ^^ {
        _.mkString
      }
    }

    lazy val TERMINATOR: Parser[String] = rule {
      (LINEFEED | SEMICOLON | EOF) << SPACING
    }


    lazy val SEPARATOR: Parser[String] = rule {
      (LINEFEED | COMMA | EOF | SPACING_WITHOUT_LF) << SPACING
    }

    lazy val SEPARATOR_WITHOUT_LF: Parser[String] = rule(
      (COMMA | EOF | SPACING_WITHOUT_LF)
    )

    lazy val BLOCK_COMMENT: Parser[Any] = rule {
      "/*" ~ (not("*/") ~ (BLOCK_COMMENT | ANY)).* ~ "*/"
    }

    lazy val LINE_COMMENT: Parser[Any] = rule {
      "//" ~ (not(LINEFEED) ~ ANY).* ~ LINEFEED
    }

    lazy val COMMENT: Parser[Any] = rule {
      BLOCK_COMMENT | LINE_COMMENT
    }

    def CL[T](parser: Parser[T]): Parser[T] = parser << SPACING

    def token(parser: String): Parser[String] = parser << SPACING_WITHOUT_LF

    def unescape(input: String): String = {
      val builder = new java.lang.StringBuilder
      val length = input.length
      var i = 0
      while (i < length - 1) {
        (input.charAt(i), input.charAt(i + 1)) match {
          case ('\\', 'r') => builder.append('\r'); i += 2
          case ('\\', 'n') => builder.append('\n'); i += 2
          case ('\\', 'b') => builder.append('\b'); i += 2
          case ('\\', 'f') => builder.append('\f'); i += 2
          case ('\\', 't') => builder.append('\t'); i += 2
          case ('\\', '\\') => builder.append('\\'); i += 2
          case (ch, _) => builder.append(ch); i += 1
        }
      }
      if (i == length - 1) {
        builder.append(input.charAt(i))
      }
      new String(builder)
    }

    lazy val LT: Parser[String] = token("<")
    lazy val GT: Parser[String] = token(">")
    lazy val LTE: Parser[String] = token("<=")
    lazy val GTE: Parser[String] = token(">=")
    lazy val UNDERSCORE: Parser[String] = token("_")
    lazy val PLUS: Parser[String] = token("+")
    lazy val MINUS: Parser[String] = token("-")
    lazy val ASTER: Parser[String] = token("*")
    lazy val SLASH: Parser[String] = token("/")
    lazy val MAT_PLUS: Parser[String] = token("_+_")
    lazy val MAT_ASTER: Parser[String] = token("_*_")
    lazy val LPAREN: Parser[String] = token("(")
    lazy val RPAREN: Parser[String] = token(")")
    lazy val LBRACE: Parser[String] = token("{")
    lazy val RBRACE: Parser[String] = token("}")
    lazy val LBRACKET: Parser[String] = token("[")
    lazy val RBRACKET: Parser[String] = token("]")
    lazy val SHARP: Parser[String] = token("#")
    lazy val IF: Parser[String] = token("if")
    lazy val ELSE: Parser[String] = token("else")
    lazy val THEN: Parser[String] = token("then")
    lazy val WHILE: Parser[String] = token("while")
    lazy val IMPORT: Parser[String] = token("import")
    lazy val ENUM: Parser[String] = token("enum")
    lazy val TRUE: Parser[String] = token("true")
    lazy val FALSE: Parser[String] = token("false")
    lazy val IN: Parser[String] = token("in")
    lazy val COMMA: Parser[String] = token(",")
    lazy val DOT: Parser[String] = token(".")
    lazy val CLASS: Parser[String] = token("class")
    lazy val RECORD: Parser[String] = token("record")
    lazy val DEF: Parser[String] = token("def")
    lazy val MUTABLE: Parser[String] = token("mutable")
    lazy val CLEANUP: Parser[String] = token("cleanup")
    lazy val VAL: Parser[String] = token("val")
    lazy val EQ: Parser[String] = token("=")
    lazy val RULE: Parser[String] = token("rule")
    lazy val BEGIN_MSTR: Parser[String] = token("<<<")
    lazy val END_MSTR: Parser[String] = token(">>>")
    lazy val COLONGT: Parser[String] = token(":>")
    lazy val EQEQ: Parser[String] = token("==")
    lazy val ARROW1: Parser[String] = token("=>")
    lazy val ARROW2: Parser[String] = token("->")
    lazy val COLON: Parser[String] = token(":")
    lazy val NEW: Parser[String] = token("new")
    lazy val QUES: Parser[String] = token("?")
    lazy val AMP2: Parser[String] = token("&&")
    lazy val BAR2: Parser[String] = token("||")
    lazy val BAR: Parser[String] = token("|")
    lazy val KEYWORDS: Set[String] = Set(
      "<", "_", ">", "<=", ">=", "+", "-", "*", "/", "{", "}", "[", "]", ":", "?", "_+_", "_*_",
      "if", "rule", "then", "else", "while",  "true", "false", "in", ",", ".",
      "class", "def", "val", "mutable", "record", "=", "==", "=>", "new", "&", "|", "&&", "||"
    )

    lazy val typeAnnotation: Parser[Type] = COLON >> typeDescription

    lazy val castType: Parser[Type] = typeDescription

    def isBuiltinType(name: String): Boolean = name match {
      case "Byte" => true
      case "Short" => true
      case "Int" => true
      case "Long" => true
      case "Float" => true
      case "Double" => true
      case "Boolean" => true
      case "Unit" => true
      case "String" => true
      case _ => false
    }

    lazy val typeVariable: Parser[TVariable] = qident ^^ { id => TVariable(id) }

    lazy val typeDescription: Parser[Type] = rule(
      qident ^^ { id => TVariable(id) }
    | ((CL(LPAREN) >> typeDescription.repeat0By(CL(COMMA)) << CL(RPAREN)) << CL(ARROW1)) ~ typeDescription ^^ { case args ~ returnType => TFunction(args, returnType) }
    | sident.filter { s => !isBuiltinType(s) } ~ (CL(LT) >> typeDescription.repeat0By(CL(COMMA)) << CL(GT)).? ^^ {
        case name ~ Some(args) => TConstructor(name, args)
        case name ~ None => TConstructor(name, Nil)
      }
    | integerLiteral ^^ {v => TLiteral(v.asInstanceOf[IntNode].value)}
    | token("Int") ^^ { _ => TInt }
    | token("Boolean") ^^ { _ => TBoolean }
    | token("Unit") ^^ { _ => TUnit }
    | token("String") ^^ { _ => TString }
    | token("*") ^^ { _ => TDynamic }
    )

    def root: Parser[Program] = rule(program)

    lazy val program: Parser[Program] = rule {
      (SPACING >> %%) ~ (lines << (TERMINATOR).?) << EOF ^^ {
        case location ~ block => Program(location, block)
      }
    }

    //lines ::= line {TERMINATOR expr} [TERMINATOR]
    lazy val lines: Parser[Block] = rule {
      SPACING >> (%% ~ line.repeat0By(TERMINATOR) << TERMINATOR.? ^^ { case location ~ expressions =>
        Block(location, expressions)
      })
    }

    //line ::= expression | valDeclaration | functionDefinition
    lazy val line: Parser[Ast.Node] = rule(expression | valDeclaration | functionDefinition)

    //expression ::= assignment | ternary | ifExpression | whileEpression
    lazy val expression: Parser[Ast.Node] = rule(assignment | ternary | ifExpression | whileExpression)

    //ifExpression ::= "if" "(" expression ")" expression "else" expression
    lazy val ifExpression: Parser[Ast.Node] = rule {
      (%% << CL(IF) << CL(LPAREN)) ~ commit(expression ~ CL(RPAREN) ~ expression ~ CL(ELSE) ~ expression) ^^ {
        case location ~ (condition ~ _ ~ positive ~ _ ~ negative) => IfExpression(location, condition, positive, negative)
      }
    }

    //whileExpression ::= "while" "(" expression ")" expression
    lazy val whileExpression: Parser[Ast.Node] = rule {
      (%% << CL(WHILE) << CL(LPAREN)) ~ commit(expression ~ (CL(RPAREN) >> expression)) ^^ {
        case location ~ (condition ~ body) => WhileExpression(location, condition, body)
      }
    }

    lazy val ternary: Parser[Ast.Node] = rule {
      for(
        location <- %%;
        condition <- infix;
        opt <- ((CL(THEN) ~> infix) ~ (CL(ELSE) ~> infix)).?
      ) yield opt match {
        case Some(th ~ el) => TernaryExpression(location, condition, th, el)
        case None => condition
      }
    }

    lazy val infix: Parser[Ast.Node] = rule {
      chainl(logical)(
        (%% ~ CL(operator)) ^^ { case location ~ op => (lhs: Ast.Node, rhs: Ast.Node) => FunctionCall(location, FunctionCall(location, Id(location, op), List(lhs)), List(rhs)) }
          | (%% ~ CL(selector)) ^^ { case location ~ sl => (lhs: Ast.Node, rhs: Ast.Node) => FunctionCall(location, FunctionCall(location, sl, List(lhs)), List(rhs)) }
      )
    }

    lazy val logical: Parser[Ast.Node] = rule {
      chainl(conditional)(
        (%% << CL(AMP2)) ^^ { location => (lhs: Ast.Node, rhs: Ast.Node) => BinaryExpression(location, Operator.AND2, lhs, rhs) }
          | (%% << CL(BAR2)) ^^ { location => (lhs: Ast.Node, rhs: Ast.Node) => BinaryExpression(location, Operator.BAR2, lhs, rhs) }
      )
    }

    //conditional ::= add {"==" add | "<=" add | "=>" add | "<" add | ">" add}
    lazy val conditional: Parser[Ast.Node] = rule {
      chainl(add)(
        (%% << CL(EQEQ)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.EQUAL, left, right) } |
          (%% << CL(LTE)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.LESS_OR_EQUAL, left, right) } |
          (%% << CL(GTE)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.GREATER_EQUAL, left, right) } |
          (%% << CL(LT)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.LESS_THAN, left, right) } |
          (%% << CL(GT)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.GREATER_THAN, left, right) }
      )
    }

    //add ::= term {"+" term | "-" term}
    lazy val add: Parser[Ast.Node] = rule {
      chainl(term)(
        (%% << CL(PLUS)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.ADD, left, right) } |
        (%% << CL(MINUS)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.SUBTRACT, left, right) } |
        (%% << CL(MAT_PLUS)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.MAT_ADD, left, right) }
      )
    }

    //term ::= factor {"*" factor | "/" factor}
    lazy val term: Parser[Ast.Node] = rule {
      chainl(unary)(
        (%% << CL(ASTER)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.MULTIPLY, left, right) } |
        (%% << CL(MAT_ASTER)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.MAT_PRODUCT, left, right) } |
        (%% << CL(SLASH)) ^^ { location => (left: Ast.Node, right: Ast.Node) => BinaryExpression(location, Operator.DIVIDE, left, right) }
      )
    }

    lazy val unary: Parser[Ast.Node] = rule(
      %% ~ MINUS ~ unary ^^ { case location ~ _ ~ operand => MinusOp(location, operand) }
      | %% ~ PLUS ~ unary ^^ { case location ~ _ ~ operand => PlusOp(location, operand) }
      | application
    )

    lazy val application: Parser[Ast.Node] = rule {
      %% ~ pipelinable ~ (
        blockFunctionParameter
          | parenthesizedParameter
        ).* ^^ {
        case location ~ f ~ params =>
          params.foldLeft(f: Ast.Node) { (f, params) =>
            FunctionCall(location, f, params)
          }
      }
    }

    lazy val pipelinable: Parser[Ast.Node] = rule(%% ~ castable ~ (CL(BAR) >> ident).? ^^ {
      case location ~ self ~ None =>
        self
      case location ~ self ~ Some(name) =>
        FunctionCall(location, name, List(self))
    })

    lazy val castable: Parser[Ast.Node] = rule(primary ~ ((%% << CL(COLONGT)) ~ CL(castType)).? ^^ {
      case target ~ Some((location ~ castType)) => Casting(location, target, castType)
      case target ~ None => target
    })

    //primary ::= selector | booleanLiteral | ident |  integerLiteral | stringLiteral | listLiteral | newObject | functionLiteral | "(" expression ")" | "{" lines "}"
    lazy val primary: Parser[Ast.Node] = rule {
      (
        selector
          | booleanLiteral
          | ident
          | integerLiteral
          | newObject
          | functionLiteral
          | predict(
          '"' -> stringLiteral,
          '[' -> matrixLiteral,
          '(' -> (CL(LPAREN) >> expression << RPAREN),
          '{' -> (CL(LBRACE) >> lines << RBRACE)
        )
        )
    }

    //integerLiteral ::= ["1"-"9"] {"0"-"9"}
    lazy val integerLiteral: Parser[Ast.Node] = (%% ~ """[1-9][0-9]*|0""".r  ^^ {
      case location ~ value => IntNode(location, value.toLong.toInt)
    }) << SPACING_WITHOUT_LF

    lazy val booleanLiteral: Parser[Ast.Node] = %% ~ (TRUE ^^ { _ => true } | FALSE ^^ { _ => false }) ^^ {
      case location ~ true => BooleanNode(location, true)
      case location ~ false => BooleanNode(location, false)
    }

    //stringLiteral ::= "\"" ((?!")(\[rntfb"'\\]|[^\\]))* "\""
    lazy val stringLiteral: Parser[Ast.Node] =
      ("\"" >>
        (%% ~ """((?!("|#\{))(\\[rntfb"'\\]|[^\\]))+""".r ^^ { case location ~ in =>
          StringNode(location, unescape(in))
        } | "#{" >> expression << "}"
          ).*
        << "\"" ^^ { values =>
        values.foldLeft(StringNode(NoLocation, ""): Ast.Node) { (node, content) => BinaryExpression(content.location, Operator.ADD, node, content) }
      }) << SPACING_WITHOUT_LF

    lazy val matrixRow: Parser[MatrixRow] = rule(
     %%  ~ ((expression.repeat1By(SEPARATOR_WITHOUT_LF) << TERMINATOR.?)) ^^ {
       case location ~ elements =>
         MatrixRow(SourceLocation(location.line, location.column), elements)
      }
    )

    lazy val matrixLiteral: Parser[Ast.Node] = rule(%% ~ (CL(LBRACKET) >> commit(matrixRow.* << RBRACKET) ) ^^ {
      case location ~ contents =>
        MatrixLiteral(location, contents)
    })

    lazy val fqcn: Parser[String] = (ident ~ (CL(DOT) ~ ident).*) ^^ {
      case id ~ ids => ids.foldLeft(id.name) { case (a, d ~ e) => a + d + e.name }
    }

    lazy val component: Parser[String] = """[A-Za-z_][a-zA-Z0-9_]*""".r

    lazy val ident: Parser[Id] = (%% ~ component.filter { n =>
      !KEYWORDS(n)
    } ^^ { case location ~ name => Id(location, name) }) << SPACING_WITHOUT_LF

    lazy val selector: Parser[Selector] = rule(((%% ~ component ~ "#" ~ component).filter { case (_ ~ m ~ _ ~ n) =>
      (!KEYWORDS(m)) && (!KEYWORDS(n))
    } ^^ { case location ~ m ~ _ ~ n => Selector(location, m, n) }) << SPACING_WITHOUT_LF)

    lazy val qident: Parser[String] = (regularExpression("""'[A-Za-z_][a-zA-Z0-9_]*""".r).filter { n =>
      !KEYWORDS(n)
    }) << SPACING_WITHOUT_LF

    lazy val sident: Parser[String] = (regularExpression("""[A-Za-z_][a-zA-Z0-9_]*""".r).filter { n =>
      !KEYWORDS(n)
    }) << SPACING_WITHOUT_LF

    lazy val operator: Parser[String] = (regularExpression("""#[A-Za-z_][a-zA-Z0-9_]*""".r).filter { n =>
      !KEYWORDS(n.substring(1))
    }.map { n => n.substring(1) }) << SPACING_WITHOUT_LF

    lazy val assignment: Parser[Assignment] = rule((ident ~< EQ) ~ expression ^^ {
      case v ~ value => eAssignment(v.location, v.name, value)
    })

    // valDeclaration ::= "val" ident "=" expression
    lazy val valDeclaration: Parser[ValDeclaration] = rule((%% ~ CL(MUTABLE ^^ { _ => false } | VAL ^^ { _ => true })) ~ commit(ident ~ (typeAnnotation.? << CL(EQ)) ~ expression) ^^ {
      case location ~ immutable ~ (valName ~ optionalType ~ value) => ValDeclaration(location, valName.name, optionalType, value, immutable)
    })

    // parenthesizedParameter ::= "(" [param {"," param}] ")"
    lazy val parenthesizedParameter: Parser[List[Ast.Node]] = rule {
      CL(LPAREN) >> CL(expression).repeat0By(CL(COMMA)) << (SPACING << RPAREN) ^^ {
        case xs => xs
      }
    }

    // blockFunctionParameter ::= "{" [param {"," param}] "=>" expression "}"
    lazy val blockFunctionParameter: Parser[List[Ast.Node]] = rule {
      (%% << CL(LBRACE)) ~ (ident ~ typeAnnotation.?).repeat0By(CL(COMMA)) ~ (typeAnnotation.? << CL(ARROW1)) ~ (expression << RBRACE) ^^ {
        case location ~ params ~ optionalType ~ body =>
          List(
            Lambda(
              location,
              params.map {
                case name ~ Some(type_) => FormalParameterOptional(name.name, Some(type_))
                case name ~ None => FormalParameterOptional(name.name, None)
              },
              optionalType,
              body
            )
          )
      }
    }

    // functionLiteral ::= "(" [param {"," param}] ")" "=>" expression
    lazy val functionLiteral: Parser[Ast.Node] = rule(%% ~ (CL(LPAREN) >> (ident ~ typeAnnotation.?).repeat0By(CL(COMMA)) << CL(RPAREN)).? ~ (typeAnnotation.? << CL(ARROW1)) ~ expression ^^ {
      case location ~ Some(params) ~ optionalType ~ body =>
        Lambda(
          location,
          params.map {
            case name ~ Some(type_) => FormalParameterOptional(name.name, Some(type_))
            case name ~ None => FormalParameterOptional(name.name, None)
          },
          optionalType,
          body
        )
      case location ~ None ~ optionalType ~ body => Lambda(location, List(), optionalType, body)
    })

    // newObject ::= "new" fqcn "(" [param {"," param} ")"
    lazy val newObject: Parser[Ast.Node] = rule((%% << CL(NEW)) ~ commit(fqcn ~ (CL(LPAREN) >> expression.repeat0By(CL(COMMA)) << RPAREN).?) ^^ {
      case location ~ (className ~ Some(params)) => ObjectNew(location, className, params)
      case location ~ (className ~ None) => ObjectNew(location, className, List())
    })

    // functionDefinition ::= "def" ident  ["(" [param {"," param}] ")"] "=" expression
    lazy val functionDefinition: Parser[FunctionDefinition] = rule {
      (%% << CL(DEF)) ~ commit(ident ~ (CL(LPAREN) >> (ident ~ typeAnnotation.?).repeat0By(CL(COMMA)) << CL(RPAREN)).? ~ (typeAnnotation.? << CL(EQ)) ~ expression ~ (CL(CLEANUP) >> expression).?) ^^ {
        case location ~ (functionName ~ params ~ optionalType ~ body ~ cleanup) =>
          val ps = params match {
            case Some(xs) =>
              xs.map {
                case name ~ Some(annotation) => FormalParameterOptional(name.name, Some(annotation))
                case name ~ None => FormalParameterOptional(name.name, None)
              }
            case None => Nil
          }
          FunctionDefinition(
            location,
            functionName.name,
            Lambda(body.location, ps, optionalType, body),
            cleanup
          )
      }
    }
  }

  import Core._

  def parseExpression(input: String): Ast.Node = {
    parse(Core.root, input) match {
      case Result.Success(program) => program.block
      case Result.Failure(location, message) => throw new InterpreterException(s"${location}:${message}")
    }
  }

  def parseAll(input: String): Program = {
    parse(Core.root, input) match {
      case Result.Success(program) => program
      case Result.Failure(location, message) => throw new InterpreterException(s"${location}:${message}")
    }
  }

  override final val name: String = "Parser"

  override final def process(input: String): Program = parseAll(input)
}
