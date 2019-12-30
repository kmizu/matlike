package com.github.kmizu.matlike

import Ast.Program
import com.github.kmizu.matlike.Ast._
import com.github.kmizu.matlike.Type.{TBoolean, TDynamic}

/**
  * @author Kota Mizushima
  */
class SyntaxRewriter extends Processor[Ast.Program, Ast.Program] {
  object SymbolGenerator {
    private[this] var counter: Int = 0
    def symbol(): String = {
      val name = "var" + counter
      counter += 1
      name
    }
  }

  import SymbolGenerator.symbol


  def doRewrite(node: Ast.Node): Ast.Node = node match {
    case Block(location, expressions) =>
      def rewriteBlock(es: List[Ast.Node]): List[Ast.Node] = es match {
        case ValDeclaration(location, variable, type_, value, immutable) :: xs =>
          List(Let(location, variable, type_, doRewrite(value), Block(location, rewriteBlock(xs)), immutable))
        case FunctionDefinition(loation, name, expression, cleanup) :: xs =>
          List(LetRec(location, name, doRewrite(expression).asInstanceOf[Ast.Lambda], cleanup.map(doRewrite), Block(location, rewriteBlock(xs))))
        case x :: xs =>
          doRewrite(x) :: rewriteBlock(xs)
        case Nil =>
          Nil
      }
      Block(location, rewriteBlock(expressions))
    case IfExpression(location, cond, pos, neg) =>
      IfExpression(location, doRewrite(cond), doRewrite(pos), doRewrite(neg))
    case WhileExpression(location, condition, body: Ast.Node) =>
      WhileExpression(location, doRewrite(condition), doRewrite(body))
    case BinaryExpression(location, operator, lhs, rhs) =>
      BinaryExpression(location, operator, doRewrite(lhs), doRewrite(rhs))
    case MinusOp(location, operand) => MinusOp(location, doRewrite(operand))
    case PlusOp(location, operand) => PlusOp(location, doRewrite(operand))
    case literal@StringNode(location, value) => literal
    case literal@IntNode(location, value) => literal
    case literal@BooleanNode(location, value) => literal
    case node@Id(_, _) => node
    case node@Selector(_, _, _) => node
    case eAssignment(location, variable, value) => eAssignment(location, variable, doRewrite(value))
    case ValDeclaration(location, variable, optionalType, value, immutable) => ValDeclaration(location, variable, optionalType, doRewrite(value), immutable)
    case Lambda(location, params, optionalType, proc) => Lambda(location, params, optionalType, doRewrite(proc))
    case FunctionCall(location, func, params) => FunctionCall(location, doRewrite(func), params.map{doRewrite})
    case MatrixLiteral(location, rows) =>  MatrixLiteral(location, rows = rows.map({r => r.copy(elements = r.elements.map(doRewrite))}))
    case ObjectNew(location, className, params) => ObjectNew(location, className, params.map{doRewrite})
    case Casting(location, target, to) => Casting(location, doRewrite(target), to)
    case TernaryExpression(location, cond, th, el) => TernaryExpression(location, doRewrite(cond), doRewrite(th), doRewrite(el))
    case otherwise => throw RewriterPanic(otherwise.toString)
  }

  def transform(program: Ast.Program): Ast.Program = {
    program.copy(block = doRewrite(program.block).asInstanceOf[Ast.Block])
  }

  override final val name: String = "Rewriter"

  override final def process(input: Program): Program = {
    transform(input)
  }
}
