package scalox

type Value = String | Int | Boolean | Double


class LiteralVal(token: LiteralValue) {
  def value: Value = {
    token match {
      case StringToken(_, value) => value
      case NumberToken(_, value) => value
      case TrueToken(_, value) => value
      case FalseToken(_, value) => value
      case NilToken(_, value) => value
    }
  }
}

object Interpreter {

  def interpret(expr: Expr): Value = {
    expr match {
      case Literal(value) => LiteralVal(value).value

      case Unary(op, expr) => op match {
        case MinusToken(_) => interpret(expr)
        case BangToken(_)  => interpret(expr)
      }

      case Binary(expr, op, expr) => op match {
            case MinusToken(_)        => interpret(expr) - interpret(expr)
            case PlusToken(_)         => interpret(expr) + interpret(expr)
            case SlashToken(_)        => interpret(expr) / interpret(expr) 
            case StarToken(_)         => interpret(expr) * interpret(expr)
            case EqualEqualToken(_)   => interpret(expr) == interpret(expr)
            case BangEqualToken(_)    => interpret(expr) != interpret(expr)
            case LessToken(_)         => interpret(expr) < interpret(expr)
            case LessEqualToken(_)    => interpret(expr) <= interpret(expr)
            case GreaterToken(_)      => interpret(expr) > interpret(expr)
            case GreaterEqualToken(_) => interpret(expr) >= interpret(expr)
      }
    }
  }
}
