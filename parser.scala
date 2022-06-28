package scalox

type UnaryOperator = MinusToken | BangToken
type BinaryOperator = MinusToken | PlusToken | SlashToken | StarToken |
                      EqualEqualToken | BangEqualToken | LessToken |
                      LessEqualToken | GreaterToken | GreaterEqualToken
type LiteralValue = StringToken | NumberToken | TrueToken | FalseToken

sealed abstract class Expr

case class Literal(value: LiteralValue) extends Expr {
    override def toString = {
        value match {
            case StringToken(_, s) => f"'${s}'"
            case NumberToken(_, n) => f"${n}"
            case TrueToken(_)      => "true"
            case FalseToken(_)     => "false"
        }
    }
}

case class Unary(operator: UnaryOperator, value: Expr) extends Expr {
    override def toString = {
        operator match {
            case MinusToken(_) => f"(- ${value})"
            case BangToken(_)  => f"(! ${value})"
        }
    }
}

case class Binary(left: Expr, operator: BinaryOperator, right: Expr) extends Expr {
    override def toString = {
        operator match {
            case MinusToken(_)        => f"(-  ${left} ${right})"
            case PlusToken(_)         => f"(+  ${left} ${right})"
            case SlashToken(_)        => f"(/  ${left} ${right})"
            case StarToken(_)         => f"(*  ${left} ${right})"
            case EqualEqualToken(_)   => f"(== ${left} ${right})"
            case BangEqualToken(_)    => f"(!= ${left} ${right})"
            case LessToken(_)         => f"(<  ${left} ${right})"
            case LessEqualToken(_)    => f"(<= ${left} ${right})"
            case GreaterToken(_)      => f"(>  ${left} ${right})"
            case GreaterEqualToken(_) => f"(>= ${left} ${right})"
        }
    }
}

case class Grouping(expr: Expr) extends Expr {
    override def toString = f"(group ${expr})"
}
