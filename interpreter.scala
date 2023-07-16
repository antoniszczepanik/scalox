package scalox

type Value = String | Boolean | Double | Null

class LiteralVal(val value: Value) {
  def -(other: LiteralVal): LiteralVal = {
    (value, other.value) match {
      case (a: Double, b: Double) => LiteralVal(a - b)
      case (a, b) => throw PanicException(-1, s"substraction not defined for $a and $b")
    }
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
        case MinusToken(_) => interpret(expr1) - interpret(expr2)
        case binary => throw PanicException(-1, s"unimplemented: interpret $binary")
        // case PlusToken(_)         => interpret(expr) + interpret(expr)
        // case SlashToken(_)        => interpret(expr) / interpret(expr) 
        // case StarToken(_)         => interpret(expr) * interpret(expr)
        // case EqualEqualToken(_)   => interpret(expr) == interpret(expr)
        // case BangEqualToken(_)    => interpret(expr) != interpret(expr)
        // case LessToken(_)         => interpret(expr) < interpret(expr)
        // case LessEqualToken(_)    => interpret(expr) <= interpret(expr)
        // case GreaterToken(_)      => interpret(expr) > interpret(expr)
        // case GreaterEqualToken(_) => interpret(expr) >= interpret(expr)
      }
      case Grouping(expr) => interpret(expr)
    }
  }
}
