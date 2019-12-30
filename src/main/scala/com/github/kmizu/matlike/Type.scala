package com.github.kmizu.matlike

sealed abstract class Type(val image: String) {
  def ==>(returnType: Type): Type.TFunction = {
    Type.TFunction(List(this), returnType)
  }
  override def toString: String = image
}
object Type {
  implicit class RichType(args: List[Type]) {
    def ==>(returnType: Type): Type.TFunction = {
      Type.TFunction(args, returnType)
    }
  }

  case class TVariable(name: String) extends Type(name)

  case class TLiteral(val value: Int) extends Type(s"Literal[${value}]")

  case object TInt extends Type("Int")

  case object TBoolean extends Type("Boolean")

  case object TUnit extends Type("Unit")

  case object TString extends Type("String")

  case object TDynamic extends Type("*")

  case object TError extends Type("!")

  case class TFunction(paramTypes: List[Type], returnType: Type) extends Type(s"(${paramTypes.mkString(", ")}) => ${returnType}")

  case class TScheme(svariables: List[TVariable], stype: Type)

  case class TConstructor(name: String, ts: List[Type]) extends Type(name + "<" + ts.mkString(", ") + ">")
}
