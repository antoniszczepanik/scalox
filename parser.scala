package scalox

import scala.reflect.ClassTag

type UnaryOperator = MinusToken | BangToken
type BinaryOperator = SlashToken | StarToken | MinusToken | PlusToken | EqualEqualToken | BangEqualToken | LessToken | LessEqualToken | GreaterToken | GreaterEqualToken
type LiteralValue = StringToken | NumberToken | TrueToken | FalseToken | NilToken

sealed abstract class Expr

case class Literal(value: LiteralValue) extends Expr {
    override def toString = {
        value match {
            case StringToken(_, s) => f"String('${s}')"
            case NumberToken(_, n) => f"Number(${n})"
            case TrueToken(_)      => "Boolean(true)"
            case FalseToken(_)     => "Boolean(false)"
            case NilToken(_)       => "nil"
        }
    }
}

case class Unary(operator: UnaryOperator, value: Expr) extends Expr {
    override def toString = {
        operator match {
            case MinusToken(_) => f"(-${value})"
            case BangToken(_)  => f"(!${value})"
        }
    }
}

case class Binary(left: Expr, operator: BinaryOperator, right: Expr) extends Expr {
    override def toString = {
        operator match {
            case MinusToken(_)        => f"(${left} - ${right})"
            case PlusToken(_)         => f"(${left} + ${right})"
            case SlashToken(_)        => f"(${left} / ${right})"
            case StarToken(_)         => f"(${left} * ${right})"
            case EqualEqualToken(_)   => f"(${left} == ${right})"
            case BangEqualToken(_)    => f"(${left} != ${right})"
            case LessToken(_)         => f"(${left} < ${right})"
            case LessEqualToken(_)    => f"(${left} <= ${right})"
            case GreaterToken(_)      => f"(${left} > ${right})"
            case GreaterEqualToken(_) => f"(${left} >= ${right})"
        }
    }
}

case class Grouping(inner: Expr) extends Expr {
    override def toString = s"Grouping($inner)"
}

sealed trait ParsingResult
object ParsingResult {
  case class Success(expr: Expr, rest: Seq[Token]) extends ParsingResult
  case class Failure(msg: String) extends ParsingResult
}

object Parser {

  import ParsingResult._

  def parse(tokens: Seq[Token]): Expr = {
    equality(tokens) match {
      case Success(expr, rest) if rest.nonEmpty => throw PanicException(rest.head.line, s"unreachable tokens: $rest")
      case Success(expr, _) => expr
      case Failure(msg) => throw PanicException(1, s"TODO: something went wrong during parsing")
    }
  }

  private def equality(tokens: Seq[Token]): ParsingResult = {
    comparison(tokens) match {
      case Success(left, (op: (EqualEqualToken | BangEqualToken))::rest) =>
        equality(rest) match {
          case Success(right, afterBinary) => Success(Binary(left, op, right), afterBinary)
          case _ => Failure(s"unable to parse binary op: $op")
        }
      case s: Success => s
      case _ => Failure("unable to parse unary")
    }
  }

  private def comparison(tokens: Seq[Token]): ParsingResult = {
    term(tokens) match {
      case Success(left, (op: (GreaterToken | GreaterEqualToken | LessToken | LessEqualToken ))::rest) =>
        comparison(rest) match {
          case Success(right, afterBinary) => Success(Binary(left, op, right), afterBinary)
          case _ => Failure(s"unable to parse binary op: $op")
        }
      case s: Success => s
      case _ => Failure("unable to parse unary")
    }
  }

  private def term(tokens: Seq[Token]): ParsingResult = {
    factor(tokens) match {
      case Success(left, (op: (PlusToken | MinusToken))::rest) =>
        term(rest) match {
          case Success(right, afterBinary) => Success(Binary(left, op, right), afterBinary)
          case _ => Failure(s"unable to parse binary op: $op")
        }
      case s: Success => s
      case _ => Failure("unable to parse unary")
    }
  }

  private def factor(tokens: Seq[Token]): ParsingResult = {
    unary(tokens) match {
      case Success(left, (op: (StarToken | SlashToken))::rest) =>
        factor(rest) match {
          case Success(right, afterBinary) => Success(Binary(left, op, right), afterBinary)
          case _ => Failure(s"unable to parse binary op: $op")
        }
      case s: Success => s
      case _ => Failure("unable to parse unary")
    }
  }

  private def unary(tokens: Seq[Token]): ParsingResult = {
    primary(tokens) match {
      case s: Success => s
      case _ => tokens match {
        case (op: UnaryOperator)::rest =>
          primary(rest) match {
            case Success(operand, afterOperand) => Success(Unary(op, operand), afterOperand)
            case Failure(msg) => Failure(s"expected primary, got $msg")
          }
        case _ => Failure("failed to match unary")
      }
    }
  }

  private def primary(tokens: Seq[Token]): ParsingResult = {
    tokens match {
      case (literal: LiteralValue)::rest => Success(Literal(literal), rest)
      case _ => matchGrouping(tokens)
    }
  }

  private def matchGrouping(tokens: Seq[Token]): ParsingResult = {
    tokens match {
      case (_: LeftParenToken)::rest =>
        equality(rest) match {
          case Success(expr, (_: RightParenToken)::afterGrouping) =>
              Success(Grouping(expr), afterGrouping)
          case _ => Failure("failed to match grouping")
        }
      case unexpected => Failure(s"unexpected primary expression: $unexpected")
    }
  }
}
