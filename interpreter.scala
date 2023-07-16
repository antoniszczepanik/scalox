package scalox

type Value = String | Boolean | Double | Null

def getType(value: Value): String = value match {
  case _: String => "String"
  case _: Boolean => "Boolean"
  case _: Double => "Double"
  case null => "Null"
}

class LiteralVal(val value: Value) {
  def +(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a + b)
    case (a, b) => throw PanicException(-1, s"addition not defined for ${getType(a)} and ${getType(b)}")
  }

  def -(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a - b)
    case (a, b) => throw PanicException(-1, s"substraction not defined for ${getType(a)} and ${getType(b)}")
  }

  def *(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a * b)
    case (a, b) => throw PanicException(-1, s"multiplication not defined for ${getType(a)} and ${getType(b)}")
  }

  def /(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a / b)
    case (a, b) => throw PanicException(-1, s"division not defined for ${getType(a)} and ${getType(b)}")
  }

  def ==(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a == b)
    case (a, b) => throw PanicException(-1, s"cannot check equality for ${getType(a)} and ${getType(b)}")
  }

  def !=(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a != b)
    case (a, b) => throw PanicException(-1, s"cannot check equality for ${getType(a)} and ${getType(b)}")
  }

  def <(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a < b)
    case (a, b) => throw PanicException(-1, s"cannot check equality for ${getType(a)} and ${getType(b)}")
  }

  def <=(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a <= b)
    case (a, b) => throw PanicException(-1, s"cannot check equality for ${getType(a)} and ${getType(b)}")
  }

  def >(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a > b)
    case (a, b) => throw PanicException(-1, s"cannot check equality for ${getType(a)} and ${getType(b)}")
  }

  def >=(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a >= b)
    case (a, b) => throw PanicException(-1, s"cannot check equality for ${getType(a)} and ${getType(b)}")
  }
}

object Interpreter {

  def interpret(expr: Expr): LiteralVal = {
    expr match {
      case literal: Literal => LiteralVal(literal.toValue)

      case Unary(op, expr) => op match {
        case MinusToken(_) => interpret(expr)
        case BangToken(_)  => interpret(expr)
      }

      case Binary(expr1, op, expr2) => op match {
        case MinusToken(_)        => interpret(expr1) - interpret(expr2)
        case PlusToken(_)         => interpret(expr1) + interpret(expr2)
        case SlashToken(_)        => interpret(expr1) / interpret(expr2) 
        case StarToken(_)         => interpret(expr1) * interpret(expr2)
        case EqualEqualToken(_)   => interpret(expr1) == interpret(expr2)
        case BangEqualToken(_)    => interpret(expr1) != interpret(expr2)
        case LessToken(_)         => interpret(expr1) < interpret(expr2)
        case LessEqualToken(_)    => interpret(expr1) <= interpret(expr2)
        case GreaterToken(_)      => interpret(expr1) > interpret(expr2)
        case GreaterEqualToken(_) => interpret(expr1) >= interpret(expr2)
      }

      case Grouping(expr) => interpret(expr)
    }
  }
}
