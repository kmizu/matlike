package com.github.kmizu.matlike

import java.util

import scala.jdk.CollectionConverters._
import com.github.kmizu.matlike.Type._
import com.github.kmizu.matlike.TypedAst._
import com.github.kmizu.matlike.RecordEnvironment
import scala.jdk.CollectionConverters._

import scala.collection.mutable

/**
 * @author Kota Mizushima
 */
class Interpreter extends Processor[TypedAst.Program, Value] {interpreter =>
  def reportError(message: String): Nothing = {
    throw InterpreterException(message)
  }

  def findMethod(self: AnyRef, name: String, params: Array[Value]): MethodSearchResult = {
    val selfClass = self.getClass
    val nameMatchedMethods = selfClass.getMethods.filter {
      _.getName == name
    }
    val maybeUnboxedMethod = nameMatchedMethods.find { m =>
      val parameterCountMatches = m.getParameterCount == params.length
      val parameterTypes = Value.classesOfValues(params)
      val parameterTypesMatches = (m.getParameterTypes zip parameterTypes).forall{ case (arg, param) =>
        arg.isAssignableFrom(param)
      }
      parameterCountMatches && parameterTypesMatches
    }.map{m =>
      m.setAccessible(true)
      UnboxedVersionMethodFound(m)
    }
    val maybeBoxedMethod = {
      nameMatchedMethods.find{m =>
        val parameterCountMatches = m.getParameterCount == params.length
        val boxedParameterTypes = Value.boxedClassesOfValues(params)
        val boxedParameterTypesMatches = (m.getParameterTypes zip boxedParameterTypes).forall{ case (arg, param) =>
          arg.isAssignableFrom(param)
        }
        parameterCountMatches && boxedParameterTypesMatches
      }
    }.map{m =>
      m.setAccessible(true)
      BoxedVersionMethodFound(m)
    }
    maybeUnboxedMethod.orElse(maybeBoxedMethod).getOrElse(NoMethodFound)
  }

  def findConstructor(target: Class[_], params: Array[Value]): ConstructorSearchResult = {
    val constructors = target.getConstructors
    val maybeUnboxedConstructor = constructors.find{c =>
      val parameterCountMatches = c.getParameterCount == params.length
      val unboxedParameterTypes = Value.classesOfValues(params)
      val parameterTypesMatches  = (c.getParameterTypes zip unboxedParameterTypes).forall{ case (arg, param) =>
        arg.isAssignableFrom(param)
      }
      parameterCountMatches && parameterTypesMatches
    }.map{c =>
      UnboxedVersionConstructorFound(c)
    }
    val maybeBoxedConstructor = {
      constructors.find{c =>
        val parameterCountMatches = c.getParameterCount == params.length
        val boxedParameterTypes = Value.boxedClassesOfValues(params)
        val parameterTypesMatches  = (c.getParameterTypes zip boxedParameterTypes).forall{ case (arg, param) =>
          arg.isAssignableFrom(param)
        }
        parameterCountMatches && parameterTypesMatches
      }
    }.map { c =>
      BoxedVersionConstructorFound(c)
    }
    maybeUnboxedConstructor.orElse(maybeBoxedConstructor).getOrElse(NoConstructorFound)
  }

  object BuiltinEnvironment extends RuntimeEnvironment(None) {
    define("substring"){ case List(ObjectValue(s:String), begin: BoxedInt, end: BoxedInt) =>
      ObjectValue(s.substring(begin.value, end.value))
    }

    define("at") { case List(ObjectValue(s:String), index: BoxedInt) =>
      ObjectValue(s.substring(index.value, index.value + 1))
    }

    define("matches") { case List(ObjectValue(s: String), ObjectValue(regex: String)) =>
      BoxedBoolean(s.matches(regex))
    }

    define("thread") { case List(fun: FunctionValue) =>
      new Thread({() =>
          val env = new RuntimeEnvironment(fun.environment)
          interpreter.evaluate(TypedAst.FunctionCall(TDynamic, NoLocation, fun.value, Nil), env)
      }).start()
      UnitValue
    }

    define("println") { case List(param) =>
      println(param)
      param
    }

    define("printlnError") { case List(param) =>
      Console.err.println(param)
      param
    }

    define("stopwatch") { case List(fun: FunctionValue) =>
      val env = new RuntimeEnvironment(fun.environment)
      val start = System.currentTimeMillis()
      interpreter.evaluate(TypedAst.FunctionCall(TDynamic, NoLocation, fun.value, List()), env)
      val end = System.currentTimeMillis()
      BoxedInt((end - start).toInt)
    }
    define("sleep"){ case List(milliseconds: BoxedInt) =>
      Thread.sleep(milliseconds.value)
      UnitValue
    }

    define("map") { case List(ObjectValue(list: java.util.List[_])) =>
      NativeFunctionValue{
        case List(fun: FunctionValue) =>
          val newList = new java.util.ArrayList[Any]
          val env = new RuntimeEnvironment(fun.environment)
          var i = 0
          while(i < list.size()) {
            val param: Value = Value.toKlassic(list.get(i).asInstanceOf[AnyRef])
            val result: Value = performFunctionInternal(fun.value, List(ValueNode(param)), env)
            newList.add(Value.fromKlassic(result))
            i += 1
          }
          ObjectValue(newList)
      }
    }

    define("assert") { case List(BoxedBoolean(condition)) =>
      ???
    }

    define("assertResult") { case List(a: Value) =>
      NativeFunctionValue{
        case List(b: Value) => ???
      }
    }

    define("head") { case List(ObjectValue(list: java.util.List[_])) =>
      Value.toKlassic(list.get(0).asInstanceOf[AnyRef])
    }
    define("tail") { case List(ObjectValue(list: java.util.List[_])) =>
      Value.toKlassic(list.subList(1, list.size()))
    }
    define("cons") { case List(value: Value) =>
      NativeFunctionValue{ case List(ObjectValue(list: java.util.List[_])) =>
        val newList = new java.util.ArrayList[Any]
        var i = 0
        newList.add(Value.fromKlassic(value))
        while(i < list.size()) {
          newList.add(list.get(i))
          i += 1
        }
        Value.toKlassic(newList)
      }
    }
    define("size") { case List(ObjectValue(list: java.util.List[_])) =>
      BoxedInt(list.size())
    }
    define("isEmpty") { case List(ObjectValue(list: java.util.List[_])) =>
      BoxedBoolean(list.isEmpty)
    }
    define("ToDo") { case Nil => ??? }
    define("url") { case List(ObjectValue(value: String)) =>
      ObjectValue(new java.net.URL(value))
    }
    define("uri") { case List(ObjectValue(value: String)) =>
      ObjectValue(new java.net.URL(value).toURI)
    }
    define("foldLeft") { case List(ObjectValue(list: java.util.List[_])) =>
      NativeFunctionValue{ case List(init: Value) =>
        NativeFunctionValue { case List(fun: FunctionValue) =>
          val env = new RuntimeEnvironment(fun.environment)
          var i = 0
          var result: Value = init
          while(i < list.size()) {
            val params: List[TypedNode] = List(ValueNode(result), ValueNode(Value.toKlassic(list.get(i).asInstanceOf[AnyRef])))
            result = performFunctionInternal(fun.value, params, env)
            i += 1
          }
          result
        }
      }
    }
    define("desktop") { case Nil =>
      ObjectValue(java.awt.Desktop.getDesktop())
    }
    defineValue("null")(
      ObjectValue(null)
    )
  }

  object BuiltinRecordEnvironment extends RecordEnvironment() {
    define("Point")(
      "x" -> TInt,
      "y" -> TInt
    )
  }

  object BuiltinModuleEnvironment extends ModuleEnvironment() {
    private final val LIST= "List"
    private final val MAP = "Map"
    private final val SET = "Set"
    enter(LIST) {
      define("head") { case List(ObjectValue(list: java.util.List[_])) =>
        Value.toKlassic(list.get(0).asInstanceOf[AnyRef])
      }
      define("tail") { case List(ObjectValue(list: java.util.List[_])) =>
        Value.toKlassic(list.subList(1, list.size()))
      }
      define("cons") { case List(value: Value) =>
        NativeFunctionValue { case List(ObjectValue(list: java.util.List[_])) =>
          val newList = new java.util.ArrayList[Any]
          var i = 0
          newList.add(Value.fromKlassic(value))
          while (i < list.size()) {
            newList.add(list.get(i))
            i += 1
          }
          Value.toKlassic(newList)
        }
      }
      define("remove") { case List(ObjectValue(self: java.util.List[_])) =>
        NativeFunctionValue{ case List(a: Value) =>
          val newList = new java.util.ArrayList[Any]
          for(v <- self.asScala) {
            newList.add(v)
          }
          newList.remove(Value.fromKlassic(a))
          ObjectValue(newList)
        }
      }
      define("size") { case List(ObjectValue(list: java.util.List[_])) =>
        BoxedInt(list.size())
      }
      define("isEmpty") { case List(ObjectValue(list: java.util.List[_])) =>
        BoxedBoolean(list.isEmpty)
      }
      define("map") { case List(ObjectValue(list: java.util.List[_])) =>
        NativeFunctionValue{
          case List(fun: FunctionValue) =>
            val newList = new java.util.ArrayList[Any]
            val env = new RuntimeEnvironment(fun.environment)
            var i = 0
            while(i < list.size()) {
              val param: Value = Value.toKlassic(list.get(i).asInstanceOf[AnyRef])
              val result: Value = performFunctionInternal(fun.value, List(ValueNode(param)), env)
              newList.add(Value.fromKlassic(result))
              i += 1
            }
            ObjectValue(newList)
        }
      }
      define("foldLeft") { case List(ObjectValue(list: java.util.List[_])) =>
        NativeFunctionValue{ case List(init: Value) =>
          NativeFunctionValue { case List(fun: FunctionValue) =>
            val env = new RuntimeEnvironment(fun.environment)
            var i = 0
            var result: Value = init
            while(i < list.size()) {
              val params: List[TypedNode] = List(ValueNode(result), ValueNode(Value.toKlassic(list.get(i).asInstanceOf[AnyRef])))
              result = performFunctionInternal(fun.value, params, env)
              i += 1
            }
            result
          }
        }
      }
    }
    enter(MAP) {
      define("add") { case List(ObjectValue(self: java.util.Map[_, _])) =>
        NativeFunctionValue{ case List(a: Value, b: Value) =>
          val newMap = new java.util.HashMap[Any, Any]()
          for((k, v) <- self.asScala) {
            newMap.put(k, v)
          }
          newMap.put(Value.fromKlassic(a), Value.fromKlassic(b))
          ObjectValue(newMap)
        }
      }
      define("containsKey") { case List(ObjectValue(self: java.util.Map[_, _])) =>
        NativeFunctionValue{ case List(k: Value) =>
          BoxedBoolean(self.containsKey(Value.fromKlassic(k)))
        }
      }
      define("containsValue") { case List(ObjectValue(self: java.util.Map[_, _])) =>
        NativeFunctionValue{ case List(v: Value) =>
          BoxedBoolean(self.containsValue(Value.fromKlassic(v)))
        }
      }
      define("get") { case List(ObjectValue(self: java.util.Map[_, _])) =>
        NativeFunctionValue{ case List(k: Value) =>
          Value.toKlassic(self.get(Value.fromKlassic(k)).asInstanceOf[AnyRef])
        }
      }
      define("size") { case List(ObjectValue(self: java.util.Map[_, _])) =>
        BoxedInt(self.size())
      }
      define("isEmpty") { case List(ObjectValue(map: java.util.Map[_, _])) =>
        BoxedBoolean(map.isEmpty)
      }
    }
    enter(SET) {
      define("add") { case List(ObjectValue(self: java.util.Set[_])) =>
        NativeFunctionValue{ case List(a: Value) =>
          val newSet = new java.util.HashSet[Any]()
          for(v <- self.asScala) {
            newSet.add(v)
          }
          newSet.add(Value.fromKlassic(a))
          ObjectValue(newSet)
        }
      }
      define("remove") { case List(ObjectValue(self: java.util.Set[_])) =>
        NativeFunctionValue{ case List(a: Value) =>
          val newSet = new java.util.HashSet[Any]()
          for(v <- self.asScala) {
            newSet.add(v)
          }
          newSet.remove(Value.fromKlassic(a))
          ObjectValue(newSet)
        }
      }
      define("contains") { case List(ObjectValue(self: java.util.Set[_])) =>
        NativeFunctionValue { case List(a: Value) =>
          BoxedBoolean(self.contains(Value.fromKlassic(a)))
        }
      }
      define("size") { case List(ObjectValue(self: java.util.Set[_])) =>
        BoxedInt(self.size())
      }
      define("isEmpty") { case List(ObjectValue(self: java.util.Set[_])) =>
        BoxedBoolean(self.isEmpty)
      }
    }
  }

  def toList(row: Type): List[(String, Type)] = row match {
    case tv@TVariable(_) => sys.error("cannot reach here")
    case otherwise => throw TyperPanic("Unexpected: " + otherwise)
  }

  final def interpret(program: TypedAst.Program): Value = {
    interpreter.evaluate(program.block, env = BuiltinEnvironment, moduleEnv = BuiltinModuleEnvironment)
  }

  private def evaluate(node: TypedNode): Value = {
    evaluate(node, BuiltinEnvironment)
  }

  private def performFunctionInternal(func: TypedNode, params: List[TypedNode], env: RuntimeEnvironment): Value = {
    performFunction(TypedAst.FunctionCall(TDynamic, NoLocation, func, params), env)
  }

  private def performFunction(node: TypedAst.FunctionCall, env: RuntimeEnvironment): Value = node match {
    case TypedAst.FunctionCall(type_, location, function, params) =>
      evaluate(function, env) match {
        case FunctionValue(TypedAst.FunctionLiteral(type_, location, fparams, optionalType, proc), cleanup, cenv) =>
          val local = new RuntimeEnvironment(cenv)
          (fparams zip params).foreach{ case (fp, ap) =>
            local(fp.name) = evaluate(ap, env)
          }
          try {
            evaluate(proc, local)
          } finally {
            cleanup.foreach { expression =>
              evaluate(expression, local)
            }
          }
        case NativeFunctionValue(body) =>
          val actualParams = params.map{p => evaluate(p, env)}
          if(body.isDefinedAt(actualParams)) {
            body(params.map{p => evaluate(p, env)})
          } else {
            reportError("parameters are not matched to the function's arguments")
          }
        case _ =>
          reportError("unknown error")
      }
  }

  private def evaluate(node: TypedNode, env: RuntimeEnvironment, recordEnv: RecordEnvironment = BuiltinRecordEnvironment, moduleEnv: ModuleEnvironment = BuiltinModuleEnvironment): Value = {
    def evalRecursive(node: TypedNode): Value = {
      node match{
        case TypedAst.Block(type_, location, expressions) =>
          val local = new RuntimeEnvironment(Some(env))
          expressions.foldLeft(UnitValue:Value){(result, x) => evaluate(x, local)}
        case TypedAst.WhileExpression(type_, location, cond, body) =>
          while(evalRecursive(cond) == BoxedBoolean(true)) {
            evalRecursive(body)
          }
          UnitValue
        case TypedAst.IfExpression(type_, location, condition, pos, neg) =>
          evalRecursive(condition) match {
            case BoxedBoolean(true) => evalRecursive(pos)
            case BoxedBoolean(false) => evalRecursive(neg)
            case _ => reportError("type error")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.AND2, lhs, rhs) =>
          evalRecursive(lhs) match {
            case BoxedBoolean(true) => evalRecursive(rhs)
            case BoxedBoolean(false) => BoxedBoolean(false)
            case _ => reportError("type error")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.BAR2, lhs, rhs) =>
          evalRecursive(lhs) match {
            case BoxedBoolean(false) => evalRecursive(rhs)
            case BoxedBoolean(true) => BoxedBoolean(true)
            case _ => reportError("type error")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval == rval)
            case (BoxedBoolean(lval), BoxedBoolean(rval)) => BoxedBoolean(lval == rval)
            case (BoxedBoolean(lval), ObjectValue(rval:java.lang.Boolean)) => BoxedBoolean(lval == rval.booleanValue())
            case (ObjectValue(lval:java.lang.Boolean), BoxedBoolean(rval)) => BoxedBoolean(lval.booleanValue() == rval)
            case (ObjectValue(lval), ObjectValue(rval)) => BoxedBoolean(lval == rval)
            case _ => reportError("comparation must be done between same types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.LESS_THAN, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval < rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.GREATER_THAN, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval > rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.LESS_OR_EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval <= rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.GREATER_EQUAL, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedBoolean(lval >= rval)
            case _ => reportError("comparation must be done between numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.ADD, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedInt(lval + rval)
            case (ObjectValue(lval:String), rval) => ObjectValue(lval + rval)
            case (lval, ObjectValue(rval:String)) => ObjectValue(lval.toString + rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.SUBTRACT, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedInt(lval - rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.MULTIPLY, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match{
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedInt(lval * rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.DIVIDE, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (BoxedInt(lval), BoxedInt(rval)) => BoxedInt(lval / rval)
            case _ => reportError("arithmetic operation must be done between the same numeric types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.MAT_ADD, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (MatrixValue(lRows), MatrixValue(rRows)) =>
              val m = lRows.size
              val n = lRows.get(0).size
              val newMatrix = new util.ArrayList[util.List[Any]]()
              for(i <- 0 until m) {
                val newRow = new util.ArrayList[Any]()
                for (j <- 0 until n) {
                  newRow.add(lRows.get(i).get(j).asInstanceOf[Int] + rRows.get(i).get(j).asInstanceOf[Int])
                }
                newMatrix.add(newRow)
              }
              MatrixValue(newMatrix)
            case _ => reportError("matrix addition must be done between matrix types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.MAT_ADD_SCALAR, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (MatrixValue(rows), BoxedInt(value)) =>
              val newMatrix = new util.ArrayList[util.List[Any]]()
              for(i <- 0 until rows.size) {
                val newRow = new util.ArrayList[Any]()
                for (j <- 0 until rows.get(0).size) {
                  newRow.add(rows.get(i).get(j).asInstanceOf[Int] + value)
                }
                newMatrix.add(newRow)
              }
              MatrixValue(newMatrix)
            case _ => reportError("matrix addition with scalar must be done between matrix type and scalar type")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.MAT_SUBTRACT, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (MatrixValue(lRows), MatrixValue(rRows)) =>
              val m = lRows.size
              val n = lRows.get(0).size
              val newMatrix = new util.ArrayList[util.List[Any]]()
              for(i <- 0 until m) {
                val newRow = new util.ArrayList[Any]()
                for (j <- 0 until n) {
                  newRow.add(lRows.get(i).get(j).asInstanceOf[Int] - rRows.get(i).get(j).asInstanceOf[Int])
                }
                newMatrix.add(newRow)
              }
              MatrixValue(newMatrix)
            case _ => reportError("matrix subtraction must be done between matrix types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.MAT_SUBTRACT_SCALAR, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (MatrixValue(rows), BoxedInt(value)) =>
              val newMatrix = new util.ArrayList[util.List[Any]]()
              for(i <- 0 until rows.size) {
                val newRow = new util.ArrayList[Any]()
                for (j <- 0 until rows.get(0).size) {
                  newRow.add(rows.get(i).get(j).asInstanceOf[Int] - value)
                }
                newMatrix.add(newRow)
              }
              MatrixValue(newMatrix)
            case _ => reportError("matrix subtraction with scalar must be done between matrix type and scalar type")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.MAT_PRODUCT, left, right) =>
          def sigma(range: Range, block: Int => Int): Int = {
            range.map(k => block(k)).sum
          }
          (evalRecursive(left), evalRecursive(right)) match {
            case (MatrixValue(lRows), MatrixValue(rRows)) =>
              val m = lRows.get(0).size
              val newMatrix = new util.ArrayList[util.List[Any]]()
              for(i <- 0 until lRows.size) {
                val newRow = new util.ArrayList[Any]()
                for (j <- 0 until rRows.get(0).size) {
                  val e = sigma(0 until m, k => lRows.get(i).get(k).asInstanceOf[Int] * rRows.get(k).get(j).asInstanceOf[Int])
                  newRow.add(e)
                }
                newMatrix.add(newRow)
              }
              MatrixValue(newMatrix)
            case _ => reportError("matrix product must be done between matrix types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.MAT_PRODUCT_SCALAR, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (MatrixValue(rows), BoxedInt(value)) =>
              val newMatrix = new util.ArrayList[util.List[Any]]()
              for(i <- 0 until rows.size) {
                val newRow = new util.ArrayList[Any]()
                for (j <- 0 until rows.get(0).size) {
                  newRow.add(rows.get(i).get(j).asInstanceOf[Int] * value)
                }
                newMatrix.add(newRow)
              }
              MatrixValue(newMatrix)
            case _ => reportError("matrix product with scalar must be done between matrix type and scalar type")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.MAT_DIVIDE, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (MatrixValue(lRows), MatrixValue(rRows)) =>
              val m = lRows.size
              val n = lRows.get(0).size
              val newMatrix = new util.ArrayList[util.List[Any]]()
              for(i <- 0 until m) {
                val newRow = new util.ArrayList[Any]()
                for (j <- 0 until n) {
                  newRow.add(lRows.get(i).get(j).asInstanceOf[Int] / rRows.get(i).get(j).asInstanceOf[Int])
                }
                newMatrix.add(newRow)
              }
              MatrixValue(newMatrix)
            case _ => reportError("matrix subtraction must be done between matrix types")
          }
        case TypedAst.BinaryExpression(type_, location, Operator.MAT_DIVIDE_SCALAR, left, right) =>
          (evalRecursive(left), evalRecursive(right)) match {
            case (MatrixValue(rows), BoxedInt(value)) =>
              val newMatrix = new util.ArrayList[util.List[Any]]()
              for(i <- 0 until rows.size) {
                val newRow = new util.ArrayList[Any]()
                for (j <- 0 until rows.get(0).size) {
                  newRow.add(rows.get(i).get(j).asInstanceOf[Int] / value)
                }
                newMatrix.add(newRow)
              }
              MatrixValue(newMatrix)
            case _ => reportError("matrix division with scalar must be done between matrix type and scalar type")
          }
        case TypedAst.MinusOp(type_, location, operand) =>
          evalRecursive(operand) match {
            case BoxedInt(value) => BoxedInt(-value)
            case _ => reportError("- cannot be applied to non-integer value")
          }
        case TypedAst.PlusOp(type_, location, operand) =>
          evalRecursive(operand) match {
            case BoxedInt(value) => BoxedInt(value)
            case _ => reportError("+ cannot be applied to non-integer value")
          }
        case TypedAst.IntNode(type_, location, value) =>
          BoxedInt(value)
        case TypedAst.StringNode(type_, location, value) =>
          ObjectValue(value)
        case TypedAst.BooleanNode(type_, location, value) =>
          BoxedBoolean(value)
        case TypedAst.MatrixLiteral(type_, location, rows) =>
          val newRows = rows.map { row =>
            row.elements.map { e => Value.fromKlassic(evalRecursive(e)) }.asJava
          }.asJava
          MatrixValue(newRows.asInstanceOf[util.List[util.List[Any]]])
        case TypedAst.Id(type_, location, name) =>
          env(name)
        case TypedAst.Selector(type_, location, module, name) =>
          moduleEnv(module)(name)
        case TypedAst.LetDeclaration(type_, location, vr, optVariableType, value, body, immutable) =>
          env(vr) = evalRecursive(value)
          evalRecursive(body)
        case TypedAst.Assignment(type_, location, vr, value) =>
          env.set(vr, evalRecursive(value))
        case literal@TypedAst.FunctionLiteral(type_, location, _, _, _) =>
          FunctionValue(literal, None, Some(env))
        case TypedAst.LetFunctionDefinition(type_, location, name, body, cleanup, expression) =>
          env(name) = FunctionValue(body, cleanup, Some(env)): Value
          evalRecursive(expression)
        case TypedAst.ObjectNew(type_, location, className, params) =>
          val paramsArray = params.map{evalRecursive}.toArray
          findConstructor(Class.forName(className), paramsArray) match {
            case UnboxedVersionConstructorFound(constructor) =>
              val actualParams = paramsArray.map{Value.fromKlassic}
              Value.toKlassic(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
            case BoxedVersionConstructorFound(constructor) =>
              val actualParams = paramsArray.map{Value.fromKlassic}
              Value.toKlassic(constructor.newInstance(actualParams:_*).asInstanceOf[AnyRef])
            case NoConstructorFound =>
              throw new IllegalArgumentException(s"new ${className}(${params}) is not found")
          }
        case call@TypedAst.FunctionCall(type_, location, function, params) =>
          performFunction(call, env)
        case TypedAst.Casting(type_, location, target, to) =>
          evalRecursive(target)
        case TypedAst.ValueNode(value) =>
          value
        case otherwise => sys.error(s"cannot reach here: ${otherwise}")
      }
    }
    evalRecursive(node)
  }

  override final val name: String = "Interpreter"

  override final def process(input: TypedAst.Program): Value = {
    interpret(input)
  }
}
