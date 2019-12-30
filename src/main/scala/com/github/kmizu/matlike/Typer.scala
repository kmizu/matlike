package com.github.kmizu.matlike

import com.github.kmizu.matlike.TypedAst.TypedNode
import com.github.kmizu.matlike.Type._
import com.github.kmizu.matlike.Ast._

import scala.collection.mutable

/**
  * @author Kota Mizushima
  */
class Typer extends Processor[Ast.Program, TypedAst.Program] {
  type ModuleEnvironment = Map[String, Environment]
  type Name = String
  type Label = String
  def listOf(tp: Type): TConstructor = {
    TConstructor("List", List(tp))
  }
  def matrixOf(element: Type, row: Type, col: Type): TConstructor = {
    TConstructor("Matrix", List(element, row, col))
  }
  val BuiltinEnvironment: Environment = {
    Map(
      "url"          -> TScheme(Nil, TFunction(List(TString), TDynamic)),
      "uri"          -> TScheme(Nil, TFunction(List(TString), TDynamic)),
      "substring"    -> TScheme(Nil, TFunction(List(TString, TInt, TInt), TString)),
      "at"           -> TScheme(Nil, TFunction(List(TDynamic, TInt), TDynamic)),
      "matches"      -> TScheme(Nil, TFunction(List(TString, TString), TBoolean)),
      "thread"       -> TScheme(Nil, TFunction(List(TFunction(List.empty, TDynamic)), TDynamic)),
      "println"      -> TScheme(List(tv("x")), TFunction(List(tv("x")), TUnit)),
      "printlnError" -> TScheme(List(tv("x")), TFunction(List(tv("x")), TUnit)),
      "stopwatch"    -> TScheme(Nil, TFunction(List(TFunction(List.empty, TDynamic)), TInt)),
      "sleep"        -> TScheme(Nil, TInt ==> TUnit),
      "isEmpty"      -> TScheme(List(tv("a")), listOf(tv("a")) ==> TBoolean),
      "ToDo"         -> TScheme(List(tv("a")), TFunction(Nil, tv("a"))),
      "assert"       -> TScheme(List(tv("a")), TBoolean ==> TUnit),
      "assertResult" -> TScheme(List(tv("a")), tv("a") ==> (tv("a") ==> TUnit)),
      "map"          -> TScheme(List(tv("a"), tv("b")), listOf(tv("a")) ==> ((tv("a") ==> tv("b"))  ==> listOf(tv("b")))),
      "head"         -> TScheme(List(tv("a")), listOf(tv("a")) ==> tv("a")),
      "tail"         -> TScheme(List(tv("a")), listOf(tv("a")) ==> listOf(tv("a"))),
      "cons"         -> TScheme(List(tv("a")), tv("a") ==> (listOf(tv("a")) ==> listOf(tv("a")))),
      "size"         -> TScheme(List(tv("a")), listOf(tv("a")) ==> TInt),
      "foldLeft"     -> TScheme(List(tv("a"), tv("b")), listOf(tv("a")) ==> (tv("b") ==> ((List(tv("b"), tv("a")) ==> tv("b")) ==> tv("b")))),
      "null"         -> TScheme(List(tv("a")), tv("a")),
      "desktop"      -> TScheme(Nil, Nil ==> TDynamic)
    )
  }

  val BuiltinModuleEnvironment: Map[String, Environment] = {
    Map(
      "List" -> Map(
        "cons" -> TScheme(List(tv("a")), TFunction(List(tv("a"), listOf(tv("a"))), listOf(tv("a")))),
        "map" -> TScheme(List(tv("a"), tv("b")), listOf(tv("a")) ==> ((tv("a") ==> tv("b"))  ==> listOf(tv("b")))),
        "head" -> TScheme(List(tv("a")), listOf(tv("a")) ==> tv("a")),
        "tail" -> TScheme(List(tv("a")), listOf(tv("a")) ==> listOf(tv("a"))),
        "size" -> TScheme(List(tv("a")), listOf(tv("a")) ==> TInt),
        "isEmpty" -> TScheme(List(tv("a")), listOf(tv("a")) ==> TBoolean)
      )
    )
  }

  def newInstanceFrom(scheme: TScheme): Type = {
    scheme.svariables.foldLeft(EmptySubstitution)((s, tv) => s.extend(tv, newTypeVariable())).replace(scheme.stype)
  }
  private var n: Int = 0
  private var m: Int = 0

  def newTypeVariable(): Type = {
    n += 1; TVariable("'a" + n)
  }

  def newTypeVariable(name: String) = {
    m += 1; TVariable(name + m)
  }

  val EmptySubstitution: Substitution = Map.empty

  def lookup(x: String, environment: Environment): Option[TScheme] = environment.get(x) match {
    case Some(t) => Some(t)
    case None => None
  }

  def generalize(t: Type, environment: Environment): TScheme = {
    TScheme(typeVariables(t) diff typeVariables(environment), t)
  }

  def unify(t: Type, u: Type, s: Substitution): Substitution = (s.replace(t), s.replace(u)) match {
    case (TVariable(a), TVariable(b)) if a == b =>
      s
    case (TVariable(a), _) if !(typeVariables(u) contains a) =>
      s.extend(TVariable(a), u)
    case (_, TVariable(a)) =>
      unify(u, t, s)
    case (TInt, TInt) =>
      s
    case (TLiteral(v1), TLiteral(v2)) if v1 == v2 =>
      s
    case (TBoolean, TBoolean) =>
      s
    case (TString, TString) =>
      s
    case (TString, TDynamic) =>
      s
    case (TDynamic, TString) =>
      s
    case (TUnit, TUnit) =>
      s
    case (TDynamic, TDynamic) =>
      s
    case (TFunction(t1, t2), TFunction(u1, u2)) if t1.size == u1.size =>
      unify(t2, u2, (t1 zip u1).foldLeft(s){ case (s, (t, u)) => unify(t, u, s)})
    case (TConstructor(k1, ts), TConstructor(k2, us)) if k1 == k2 =>
      (ts zip us).foldLeft(s){ case (s, (t, u)) => unify(t, u, s)}
    case _ =>
      typeError(current.location, s"cannot unify ${s.replace(t)} with ${s.replace(u)}")
  }

  def toList(row: Type): (List[(String, Type)], Option[TVariable]) = row match {
    case tv@TVariable(_) => (Nil, Some(tv))
    case otherwise => throw TyperPanic("Unexpected: " + otherwise)
  }

  def typeOf(e: Ast.Node, environment: Environment = BuiltinEnvironment, modules: ModuleEnvironment = BuiltinModuleEnvironment): Type = {
    val a = newTypeVariable()
    val r = new SyntaxRewriter
    val (typedE, s) = doType(r.doRewrite(e), TypeEnvironment(environment, Set.empty, modules, None), a, EmptySubstitution)
    s.replace(a)
  }

  var current: Ast.Node = null
  var recordEnvironment: RecordEnvironment = null
  def doType(e: Ast.Node, env: TypeEnvironment, t: Type, s0: Substitution): (TypedNode, Substitution) = {
    current = e
    e match {
      case Ast.Block(location, expressions) =>
        expressions match {
          case Nil =>
            (TypedAst.Block(TUnit, location, Nil), s0)
          case x::Nil =>
            val (typedX, newSub) = doType(x, env, t, s0)
            (TypedAst.Block(newSub.replace(t), location, typedX::Nil), newSub)
          case x::xs =>
            val t = newTypeVariable()
            val ts = xs.map{_ => newTypeVariable()}
            val (result, s1) = doType(x, env, t, s0)
            val (reversedTypedElements, s2) = (xs zip ts).foldLeft((result::Nil, s1)){ case ((a, s), (e, t)) =>
              val (e2, s2) = doType(e, env, t, s)
              (e2::a, s2)
            }
            (TypedAst.Block(s2.replace(ts.last), location, reversedTypedElements.reverse), s2)
        }
      case Ast.IntNode(location, value) =>
        val newSub = unify(t, TInt, s0)
        (TypedAst.IntNode(newSub.replace(t), location, value), newSub)
      case Ast.BooleanNode(location, value) =>
        val newSub = unify(t, TBoolean, s0)
        (TypedAst.BooleanNode(newSub.replace(t), location, value), newSub)
      case Ast.eAssignment(location, variable, value) =>
        if(env.immutableVariables.contains(variable)) {
          typeError(location, s"variable '$variable' cannot change")
        }
        env.lookup(variable) match {
          case None =>
            typeError(location, s"variable $variable is not defined")
          case Some(variableType) =>
            val (typedValue, s1) = doType(value, env, t, s0)
            val s2 = unify(variableType.stype, typedValue.type_, s1)
            (TypedAst.Assignment(variableType.stype, location, variable, typedValue), s2)
        }
      case Ast.IfExpression(location, cond, pos, neg) =>
        val (typedCondition, newSub1) = doType(cond, env, TBoolean, s0)
        val (posTyped, newSub2) = doType(pos, env, t, newSub1)
        val (negTyped, newSub3) = doType(neg, env, t, newSub2)
        (TypedAst.IfExpression(newSub3.replace(t), location, typedCondition, posTyped, negTyped), newSub3)
      case Ast.TernaryExpression(location, cond, pos, neg) =>
        val (typedCondition, newSub1) = doType(cond, env, TBoolean, s0)
        val (posTyped, newSub2) = doType(pos, env, t, newSub1)
        val (negTyped, newSub3) = doType(neg, env, t, newSub2)
        (TypedAst.IfExpression(newSub3.replace(t), location, typedCondition, posTyped, negTyped), newSub3)
      case Ast.WhileExpression(location, condition, body) =>
        val a = newTypeVariable()
        val b = newTypeVariable()
        val c = newTypeVariable()
        val (typedCondition, s1) = doType(condition, env, a, s0)
        if(typedCondition.type_ != TBoolean) {
          typeError(location, s"condition type must be Boolean, actual: ${typedCondition.type_}")
        } else {
          val (typedBody, s2) = doType(body, env, b, s1)
          val s3 = unify(TUnit, t, s2)
          (TypedAst.WhileExpression(TUnit, location, typedCondition, typedBody), s3)
        }
      case Ast.BinaryExpression(location, Operator.EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
          case (TInt, TInt) =>
            (TBoolean, s2)
          case (TBoolean, TBoolean) =>
            (TBoolean, s2)
          case (TString, TString) =>
            (TBoolean, s2)
          case (TString, TDynamic) =>
            (TBoolean, s2)
          case (TDynamic, TString) =>
            (TBoolean, s2)
          case (TDynamic, TDynamic) =>
            (TBoolean, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (a@TConstructor(n1, ts1), b@TConstructor(n2, ts2)) if n2 == n2  && ts1.length == ts2.length =>
            val sx = (ts1 zip ts2).foldLeft(s0) { case (s, (t1, t2)) =>
                unify(t1, t2, s)
            }
            (sx.replace(a), sx)
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TBoolean, s4)
        }
        val s4 = unify(TBoolean, t, s3)
        (TypedAst.BinaryExpression(resultType, location, Operator.EQUAL, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.LESS_THAN, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
          case (TInt, TInt) =>
            (TBoolean, s2)
          case (TDynamic, TDynamic) =>
            (TBoolean, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TBoolean, s4)
        }
        val s4 = unify(TBoolean, t, s3)
        (TypedAst.BinaryExpression(resultType, location, Operator.LESS_THAN, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.GREATER_THAN, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
          case (TInt, TInt) =>
            (TBoolean, s2)
          case (TDynamic, TDynamic) =>
            (TBoolean, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TBoolean, s4)
        }
        val s4 = unify(TBoolean, t, s3)
        (TypedAst.BinaryExpression(resultType, location, Operator.GREATER_THAN, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.LESS_OR_EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
          case (TInt, TInt) =>
            (TBoolean, s2)
            (TBoolean, s2)
          case (TDynamic, TDynamic) =>
            (TBoolean, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TBoolean, s4)
        }
        val s4 = unify(TBoolean, t, s3)
        (TypedAst.BinaryExpression(resultType, location, Operator.LESS_OR_EQUAL, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.GREATER_EQUAL, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
          case (TInt, TInt) =>
            (TBoolean, s2)
          case (TDynamic, TDynamic) =>
            (TBoolean, s2)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (TBoolean, unify(x, y, s2))
          case (ltype, rtype) =>
            val s3 = unify(TInt, ltype, s2)
            val s4 = unify(TInt, rtype, s3)
            (TBoolean, s4)
        }
        val s4 = unify(TBoolean, t, s3)
        (TypedAst.BinaryExpression(resultType, location, Operator.GREATER_EQUAL, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.ADD, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        (s2.replace(a), s2.replace(b)) match {
          case (TInt, TInt) =>
            val resultType = TInt
            val sLast = unify(resultType, t, s2)
            (TypedAst.BinaryExpression(resultType, location, Operator.ADD, typedLhs, typedRhs), sLast)
          case (TString, other) =>
            val resultType = TString
            val sLast = unify(resultType, t, s2)
            (TypedAst.BinaryExpression(resultType, location, Operator.ADD, typedLhs, typedRhs), sLast)
          case (other, TString) =>
            val resultType = TString
            val sLast = unify(resultType, t, s2)
            (TypedAst.BinaryExpression(resultType, location, Operator.ADD, typedLhs, typedRhs), sLast)
          case (TDynamic, other) =>
            val resultType = TDynamic
            val sLast = unify(resultType, t, s2)
            (TypedAst.BinaryExpression(resultType, location, Operator.ADD, typedLhs, typedRhs), sLast)
          case (other, TDynamic) =>
            val resultType = TDynamic
            val sLast = unify(resultType, t, s2)
            (TypedAst.BinaryExpression(resultType, location, Operator.ADD, typedLhs, typedRhs), sLast)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            val resultType = y
            val s3 = unify(x, y, s2)
            val sLast = unify(resultType, t, s3)
            (TypedAst.BinaryExpression(resultType, location, Operator.ADD, typedLhs, typedRhs), sLast)
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            val resultType = x
            val s3 = unify(x, y, s2)
            val sLast = unify(resultType, t, s3)
            (TypedAst.BinaryExpression(resultType, location, Operator.ADD, typedLhs, typedRhs), sLast)
          case (TConstructor("Matrix", List(elem1, row1, col1)), TConstructor("Matrix", List(elem2, row2, col2))) =>
            var sx = s2
            sx = unify(elem1, elem2, sx)
            sx = unify(row1, row2, sx)
            sx = unify(col1, col2, sx)
            val resultType = matrixOf(elem1, row1, col1)
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.MAT_ADD, typedLhs, typedRhs), sLast)
          case (TConstructor("Matrix", List(elem, row, col)), TInt) =>
            var sx = s2
            sx = unify(elem, TInt, sx)
            val resultType = matrixOf(elem, row, col)
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.MAT_ADD_SCALAR, typedLhs, typedRhs), sLast)
          case (TInt, (TConstructor("Matrix", List(elem1, row, col)))) =>
            var sx = s2
            sx = unify(elem1, TInt, sx)
            val resultType = matrixOf(elem1, row, col)
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.MAT_ADD_SCALAR, typedRhs, typedLhs), sLast)
          case (lType, rType) =>
            var sx = s2
            sx = unify(lType, TInt, sx)
            sx = unify(rType, TInt, sx)
            val resultType = TInt
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.ADD, typedLhs, typedRhs), sLast)
        }
      case Ast.BinaryExpression(location, Operator.SUBTRACT, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        (s2.replace(a), s2.replace(b)) match {
          case (TInt, TInt) =>
            val resultType = TInt
            val sLast = unify(resultType, t, s2)
            (TypedAst.BinaryExpression(resultType, location, Operator.SUBTRACT, typedLhs, typedRhs), sLast)
          case (TDynamic, TDynamic) =>
            val resultType = TDynamic
            val sLast = unify(resultType, t, s2)
            (TypedAst.BinaryExpression(resultType, location, Operator.SUBTRACT, typedLhs, typedRhs), sLast)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            val resultType = y
            val s3 = unify(x, y, s2)
            val sLast = unify(resultType, t, s3)
            (TypedAst.BinaryExpression(resultType, location, Operator.SUBTRACT, typedLhs, typedRhs), sLast)
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            val resultType = x
            val s3 = unify(x, y, s2)
            val sLast = unify(resultType, t, s3)
            (TypedAst.BinaryExpression(resultType, location, Operator.SUBTRACT, typedLhs, typedRhs), sLast)
          case (TConstructor("Matrix", List(elem1, row1, col1)), TConstructor("Matrix", List(elem2, row2, col2))) =>
            var sx = s2
            sx = unify(elem1, elem2, sx)
            sx = unify(row1, row2, sx)
            sx = unify(col1, col2, sx)
            val resultType = matrixOf(elem1, row1, col1)
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.MAT_SUBTRACT, typedLhs, typedRhs), sLast)
          case (TConstructor("Matrix", List(elem, row, col)), TInt) =>
            var sx = s2
            sx = unify(elem, TInt, sx)
            val resultType = matrixOf(elem, row, col)
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.MAT_SUBTRACT_SCALAR, typedLhs, typedRhs), sLast)
          case (TInt, (TConstructor("Matrix", List(elem1, row, col)))) =>
            var sx = s2
            sx = unify(elem1, TInt, sx)
            val resultType = matrixOf(elem1, row, col)
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.MAT_SUBTRACT_SCALAR, typedRhs, typedLhs), sLast)
          case (lType, rType) =>
            var sx = s2
            sx = unify(lType, TInt, sx)
            sx = unify(rType, TInt, sx)
            val resultType = TInt
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.SUBTRACT, typedLhs, typedRhs), sLast)
        }
      case Ast.BinaryExpression(location, Operator.MULTIPLY, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        (s2.replace(a), s2.replace(b)) match {
          case (TConstructor("Matrix", List(elem1, row1, col1)), TConstructor("Matrix", List(elem2, row2, col2))) =>
            var sx = s2
            sx = unify(elem1, elem2, sx)
            sx = unify(col1, row2, sx)
            val resultType = matrixOf(elem1, row1, col2)
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.MAT_PRODUCT, typedLhs, typedRhs), sLast)
          case (TConstructor("Matrix", List(elem1, row, col)), TInt) =>
            var sx = s2
            sx = unify(elem1, TInt, sx)
            val resultType = matrixOf(elem1, row, col)
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.MAT_PRODUCT_SCALAR, typedLhs, typedRhs), sLast)
          case (TInt, (TConstructor("Matrix", List(elem1, row, col)))) =>
            var sx = s2
            sx = unify(elem1, TInt, sx)
            val resultType = matrixOf(elem1, row, col)
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.MAT_PRODUCT_SCALAR, typedRhs, typedLhs), sLast)
          case (TInt, TInt) =>
            val resultType = TInt
            val sLast = unify(resultType, t, s2)
            (TypedAst.BinaryExpression(resultType, location, Operator.MULTIPLY, typedLhs, typedRhs), sLast)
          case (TDynamic, TDynamic) =>
            val resultType = TDynamic
            val sLast = unify(resultType, t, s2)
            (TypedAst.BinaryExpression(resultType, location, Operator.MULTIPLY, typedLhs, typedRhs), sLast)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            val resultType = y
            val s3 = unify(x, y, s2)
            val sLast = unify(resultType, t, s3)
            (TypedAst.BinaryExpression(resultType, location, Operator.MULTIPLY, typedLhs, typedRhs), sLast)
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            val resultType = x
            val s3 = unify(x, y, s2)
            val sLast = unify(resultType, t, s3)
            (TypedAst.BinaryExpression(resultType, location, Operator.MULTIPLY, typedLhs, typedRhs), sLast)
          case (ltype, rtype) =>
            val mElem1, mRow1, mCol1 = newTypeVariable()
            val mElem2, mRow2, mCol2 = newTypeVariable()
            var sx = s2
            sx = unify(ltype, matrixOf(mElem1, mRow1, mCol1), sx)
            sx = unify(rtype, matrixOf(mElem2, mRow2, mCol2), sx)
            sx = unify(mElem1, mElem2, sx)
            sx = unify(mRow2, mCol1, sx)
            val resultType = matrixOf(mElem1, mRow1, mCol2)
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.MAT_PRODUCT, typedLhs, typedRhs), sLast)
        }
      case Ast.BinaryExpression(location, Operator.DIVIDE, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        (s2.replace(a), s2.replace(b)) match {
          case (TInt, TInt) =>
            val resultType = TInt
            val sLast = unify(resultType, t, s2)
            (TypedAst.BinaryExpression(resultType, location, Operator.DIVIDE, typedLhs, typedRhs), sLast)
          case (TDynamic, TDynamic) =>
            val resultType = TDynamic
            val sLast = unify(resultType, t, s2)
            (TypedAst.BinaryExpression(resultType, location, Operator.DIVIDE, typedLhs, typedRhs), sLast)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            val resultType = y
            val s3 = unify(x, y, s2)
            val sLast = unify(resultType, t, s3)
            (TypedAst.BinaryExpression(resultType, location, Operator.DIVIDE, typedLhs, typedRhs), sLast)
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            val resultType = x
            val s3 = unify(x, y, s2)
            val sLast = unify(resultType, t, s3)
            (TypedAst.BinaryExpression(resultType, location, Operator.DIVIDE, typedLhs, typedRhs), sLast)
          case (TConstructor("Matrix", List(elem1, row1, col1)), TConstructor("Matrix", List(elem2, row2, col2))) =>
            var sx = s2
            sx = unify(elem1, elem2, sx)
            sx = unify(row1, row2, sx)
            sx = unify(col1, col2, sx)
            val resultType = matrixOf(elem1, row1, col1)
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.MAT_DIVIDE, typedLhs, typedRhs), sLast)
          case (TConstructor("Matrix", List(elem, row, col)), TInt) =>
            var sx = s2
            sx = unify(elem, TInt, sx)
            val resultType = matrixOf(elem, row, col)
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.MAT_DIVIDE_SCALAR, typedLhs, typedRhs), sLast)
          case (lType, rType) =>
            var sx = s2
            sx = unify(lType, TInt, sx)
            sx = unify(rType, TInt, sx)
            val resultType = TInt
            val sLast = unify(resultType, t, sx)
            (TypedAst.BinaryExpression(resultType, location, Operator.DIVIDE, typedLhs, typedRhs), sLast)
        }
      case Ast.MinusOp(location, operand) =>
        val a = newTypeVariable()
        val (typedOperand, s1) = doType(operand, env, a, s0)
        val (resultType, s) = s1.replace(a) match {
          case TInt  =>
            (TInt, s1)
          case TDynamic =>
            (TDynamic, s1)
          case operandType =>
            val s2 = unify(TInt, operandType, s1)
            (TInt, s2)
        }
        (TypedAst.MinusOp(resultType, location, typedOperand), s)
      case Ast.PlusOp(location, operand) =>
        val a = newTypeVariable()
        val (typedOperand, s1) = doType(operand, env, a, s0)
        val (resultType, s) = s1.replace(a) match {
          case TInt  =>
            (TInt, s1)
          case TDynamic =>
            (TDynamic, s1)
          case operandType =>
            val s2 = unify(TInt, operandType, s1)
            (TInt, s2)
        }
        (TypedAst.PlusOp(resultType, location, typedOperand), s)
      case Ast.BinaryExpression(location, Operator.MAT_PRODUCT, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, a, s0)
        val (typedRhs, s2) = doType(rhs, env, b, s1)
        val (resultType, s3) = (s2.replace(a), s2.replace(b)) match {
          case (TConstructor("Matrix", List(elem1, row1, col1)), TConstructor("Matrix", List(elem2, row2, col2))) =>
            var sx = s2
            sx = unify(elem1, elem2, sx)
            sx = unify(col1, row2, sx)
            (matrixOf(elem1, row1, col2), sx)
          case (x: TVariable, y) if !y.isInstanceOf[TVariable] =>
            (y, unify(x, y, s2))
          case (x, y: TVariable) if !x.isInstanceOf[TVariable] =>
            (x, unify(x, y, s2))
          case (ltype, rtype) =>
            val mElem1, mRow1, mCol1 = newTypeVariable()
            val mElem2, mRow2, mCol2 = newTypeVariable()
            var sx = s2
            sx = unify(ltype, matrixOf(mElem1, mRow1, mCol1), sx)
            sx = unify(rtype, matrixOf(mElem2, mRow2, mCol2), sx)
            sx = unify(mElem1, mElem2, sx)
            sx = unify(mRow2, mCol1, sx)
            (matrixOf(mElem1, mRow1, mCol2), sx)
        }
        val s4 = unify(resultType, t, s3)
        (TypedAst.BinaryExpression(resultType, location, Operator.MAT_PRODUCT, typedLhs, typedRhs), s4)
      case Ast.BinaryExpression(location, Operator.AND2, lhs, rhs) =>
        val (typedLhs, s1) = doType(lhs, env, TBoolean, s0)
        val (typedRhs, s2) = doType(rhs, env, TBoolean, s1)
        val s = unify(TBoolean, t, s2)
        (TypedAst.BinaryExpression(TBoolean, location, Operator.AND2, typedLhs, typedRhs), s)
      case Ast.BinaryExpression(location, Operator.BAR2, lhs, rhs) =>
        val a, b = newTypeVariable()
        val (typedLhs, s1) = doType(lhs, env, TBoolean, s0)
        val (typedRhs, s2) = doType(rhs, env, TBoolean, s1)
        val s = unify(TBoolean, t, s2)
        (TypedAst.BinaryExpression(TBoolean, location, Operator.BAR2, typedLhs, typedRhs), s)
      case Ast.StringNode(location, value) =>
        val s = unify(TString, t, s0)
        (TypedAst.StringNode(TString, location, value), s)
      case Ast.Id(location, name) =>
        val s = env.lookup(name) match {
          case None => typeError(location, s"variable '${name}' is not found")
          case Some(u) => unify(newInstanceFrom(u), t, s0)
        }
        val resultType = s.replace(t)
        (TypedAst.Id(resultType, location, name), s)
      case Ast.Selector(location, module, name) =>
        val s = env.lookupModuleMember(module, name) match {
          case None => typeError(location, s"module '${module}' or member '${name}' is not found")
          case Some(u) => unify(newInstanceFrom(u), t, s0)
        }
        val resultType = s.replace(t)
        (TypedAst.Selector(resultType, location, module, name), s)
      case Ast.Lambda(location, params, optionalType, body) =>
        val b = optionalType.getOrElse(newTypeVariable())
        val ts = params.map{p => p.optionalType.getOrElse(newTypeVariable())}
        val as = (params zip ts).map{ case (p, t) => p.name -> TScheme(Nil, t) }
        val s1 = unify(t, TFunction(ts, b), s0)
        val env1 = as.foldLeft(env) { case (env, (name, scheme)) => env.updateImmuableVariable(name, scheme)}
        val (typedBody, s) = doType(body, env1, b, s1)
        (TypedAst.FunctionLiteral(s.replace(t), location, params, optionalType, typedBody), s)
      case Ast.Let(location, variable, optionalType, value, body, immutable) =>
        if(env.variables.contains(variable)) {
          typeError(location, s"variable $variable is already defined")
        }
        val a = optionalType.map {
          case otherwise =>
            otherwise
        }.getOrElse(newTypeVariable())
        val (typedValue, s1) = doType(value, env, a, s0)
        val s2 = unify(s1.replace(a), typedValue.type_, s1)
        val gen = generalize(s2.replace(a), env.variables)
        val declaredType = s2.replace(a)
        val newEnv = if(immutable) {
          env.updateImmuableVariable(variable, generalize(declaredType, env.variables))
        } else {
          env.updateMutableVariable(variable, generalize(declaredType, env.variables))
        }
        val (typedBody, s) = doType(body, newEnv, t, s2)
        (TypedAst.LetDeclaration(typedBody.type_, location, variable, declaredType, typedValue, typedBody, immutable), s)
      case Ast.LetRec(location, variable, value, cleanup, body) =>
        if(env.variables.contains(variable)) {
          throw new InterruptedException(s"${location.format} function ${variable} is already defined")
        }
        val a = newTypeVariable()
        val b = newTypeVariable()
        val (typedE1, s1) = doType(value, env.updateImmuableVariable(variable, TScheme(Nil, a)), b, s0)
        val s2 = unify(a, b, s1)
        val (typedE2, s3) = doType(body, env.updateImmuableVariable(variable, generalize(s2.replace(a), s2(env.variables))), t, s2)
        val x = newTypeVariable()
        val (typedCleanup, s) = cleanup.map{c => doType(c, env, x, s3)} match {
          case Some((c, s)) => (Some(c), s)
          case None => (None, s3)
        }
        (TypedAst.LetFunctionDefinition(typedE2.type_, location, variable, typedE1.asInstanceOf[TypedAst.FunctionLiteral], typedCleanup, typedE2), s)
      case Ast.FunctionCall(location, e1, ps) =>
        val t2 = ps.map{_ => newTypeVariable()}
        val (typedTarget, s1) = doType(e1, env, TFunction(t2, t), s0)
        val (tparams, s) = (ps zip t2).foldLeft((Nil:List[TypedNode], s1)){ case ((tparams, s), (e, t)) =>
          val (tparam, sx) = doType(e, env, t, s)
          (tparam::tparams, sx)
        }
        (TypedAst.FunctionCall(s.replace(t), location, typedTarget, tparams.reverse), s)
      case Ast.MatrixLiteral(location, rows) =>
        val elementType = newTypeVariable()
        val rowType = newTypeVariable()
        val colType = newTypeVariable()
        val matrixOfA = matrixOf(elementType, rowType, colType)
        var sx: Substitution = s0
        val rowsElements = mutable.Buffer.empty[TypedAst.MatrixRow]
        for(row <- rows) {
          val rowElements = mutable.Buffer.empty[TypedAst.TypedNode]
          val numberOfElements = row.elements.size
          for(e <- row.elements) {
            val (newE, newSx) = doType(e, env, elementType, sx)
            sx = newSx
            rowElements += newE
          }
          sx = unify(colType, TLiteral(numberOfElements), sx)
          rowsElements += TypedAst.MatrixRow(rowElements.toList)
        }
        sx = unify(rowType, TLiteral(rows.size), sx)
        val s = unify(matrixOfA, t, sx)
        (TypedAst.MatrixLiteral(s.replace(t), location, rowsElements.toList), s)
      case Ast.ObjectNew(location, className, params) =>
        val ts = params.map{_ => newTypeVariable()}
        val (tes, sx) = (params zip ts).foldLeft((Nil:List[TypedNode], s0)){ case ((tes, s), (e, t)) =>
          val (te, sx) = doType(e, env, t, s)
          (te::tes, sx)
        }
        val s = unify(TDynamic, t, sx)
        (TypedAst.ObjectNew(TDynamic, location, className, tes.reverse), s)
      case Ast.Casting(location, target, to) =>
        val a = newTypeVariable()
        val (typedTarget, s1) = doType(target, env, a, s0)
        val s = unify(t, to, s1)
        (TypedAst.Casting(to, location, typedTarget, to), s)
      case otherwise =>
        throw TyperPanic(otherwise.toString)
    }
  }

  def typeError(location: Location, message: String): Nothing = {
    throw TyperException(s"${location.format} ${message}")
  }

  def transform(program: Ast.Program): TypedAst.Program = {
    val tv = newTypeVariable()
    val s: Substitution = EmptySubstitution
    val (typedExpression, _) = doType(program.block, TypeEnvironment(BuiltinEnvironment, Set.empty, BuiltinModuleEnvironment, None), tv, EmptySubstitution)
    TypedAst.Program(program.location, Nil, typedExpression.asInstanceOf[TypedAst.Block] )
  }

  override final val name: String = "Typer"

  override final def process(input: Ast.Program): TypedAst.Program = transform(input)
}
