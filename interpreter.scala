package scalox

type Value = String | Boolean | Double | Null

class LiteralVal(val value: Value, line: Int) {
  def +(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a + b, line)
    case (a: String, b: String) => LiteralVal(a.concat(b), line)
    case (a, b) => throw PanicException(line, s"addition not defined for ${getType(a)} and ${getType(b)}")
  }

  def -(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a - b, line)
    case (a, b) => throw PanicException(line, s"substraction not defined for ${getType(a)} and ${getType(b)}")
  }

  def *(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a * b, line)
    case (a, b) => throw PanicException(line, s"multiplication not defined for ${getType(a)} and ${getType(b)}")
  }

  def /(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a / b, line)
    case (a, b) => throw PanicException(line, s"division not defined for ${getType(a)} and ${getType(b)}")
  }

  def ==(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a == b, line)
    case (a: Boolean, b: Boolean) => LiteralVal(a == b, line)
    case (a: String, b: String) => LiteralVal(a == b, line)
    case (a, b) => throw PanicException(line, s"cannot check equality for ${getType(a)} and ${getType(b)}")
  }

  def !=(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a != b, line)
    case (a: Boolean, b: Boolean) => LiteralVal(a != b, line)
    case (a: String, b: String) => LiteralVal(a != b, line)
    case (a, b) => throw PanicException(line, s"cannot check equality for ${getType(a)} and ${getType(b)}")
  }

  def <(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a < b, line)
    case (a, b) => throw PanicException(line, s"cannot check equality for ${getType(a)} and ${getType(b)}")
  }

  def <=(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a <= b, line)
    case (a, b) => throw PanicException(line, s"cannot check equality for ${getType(a)} and ${getType(b)}")
  }

  def >(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a > b, line)
    case (a, b) => throw PanicException(line, s"cannot check equality for ${getType(a)} and ${getType(b)}")
  }

  def >=(other: LiteralVal): LiteralVal = (value, other.value) match {
    case (a: Double, b: Double) => LiteralVal(a >= b, line)
    case (a, b) => throw PanicException(line, s"cannot check equality for ${getType(a)} and ${getType(b)}")
  }

  def unary_! = value match {
    case v: Boolean => LiteralVal(!v, line)
    case value => throw PanicException(line, s"cannot perform boolean negation ${getType(value)}")
  }

  def unary_- = value match {
    case v: Double => LiteralVal(-v, line)
    case value => throw PanicException(line, s"cannot negate ${getType(value)}")
  }
}

object Interpreter {

  def interpret(stmt: Stmt): Unit = stmt match {
    case expr: Expr => evalExpr(expr)
    case PrintStmt(expr) => println(evalExpr(expr).value)
    case other => throw PanicException(0, s"unimplemented: interpret $other")
  }

  def evalExpr(expr: Expr): LiteralVal = {
    expr match {
      case literal: Literal => literal.toLiteralVal

      case Unary(op, expr) => op match {
        case MinusToken(_) => -evalExpr(expr)
        case BangToken(_)  => !evalExpr(expr)
      }

      case Binary(expr1, op, expr2) => op match {
        case MinusToken(_)        => evalExpr(expr1) - evalExpr(expr2)
        case PlusToken(_)         => evalExpr(expr1) + evalExpr(expr2)
        case SlashToken(_)        => evalExpr(expr1) / evalExpr(expr2) 
        case StarToken(_)         => evalExpr(expr1) * evalExpr(expr2)
        case EqualEqualToken(_)   => evalExpr(expr1) == evalExpr(expr2)
        case BangEqualToken(_)    => evalExpr(expr1) != evalExpr(expr2)
        case LessToken(_)         => evalExpr(expr1) < evalExpr(expr2)
        case LessEqualToken(_)    => evalExpr(expr1) <= evalExpr(expr2)
        case GreaterToken(_)      => evalExpr(expr1) > evalExpr(expr2)
        case GreaterEqualToken(_) => evalExpr(expr1) >= evalExpr(expr2)
      }

      case Grouping(expr) => evalExpr(expr)
    }
  }
}

def getType(value: Value): String = value match {
  case _: String => "String"
  case _: Boolean => "Boolean"
  case _: Double => "Double"
  case null => "Null"
}
