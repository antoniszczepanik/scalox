package scalox

import scala.reflect.ClassTag

type UnaryOperator = MinusToken | BangToken
type BinaryOperator = SlashToken | StarToken | MinusToken | PlusToken | EqualEqualToken | BangEqualToken | LessToken | LessEqualToken | GreaterToken | GreaterEqualToken
type LiteralValue = StringToken | NumberToken | TrueToken | FalseToken | NilToken

sealed abstract class Stmt

case class PrintStmt(expr: Expr) extends Stmt

sealed abstract class Expr extends Stmt

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

  def toLiteralVal: LiteralVal = {
    value match {
      case StringToken(line, value) => LiteralVal(value, line)
      case NumberToken(line, value) => LiteralVal(value, line)
      case TrueToken(line) => LiteralVal(true, line)
      case FalseToken(line) => LiteralVal(false, line)
      case NilToken(line) => LiteralVal(null, line)
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
  case class Success(stmt: Stmt, rest: Seq[Token]) extends ParsingResult
  case class Failure(msg: String) extends ParsingResult 
}

object Parser {

  import ParsingResult._

  def parse(tokens: Seq[Token]): Seq[Stmt] = {
    statement(tokens) match {
      case Success(stmt, Nil) => Seq(stmt)
      case Success(stmt, rest) => Seq(stmt) ++ parse(rest)
      case failure: Failure => throw PanicException(1, s"parse error: ${failure.msg}")
    }
  }

  private def statement(tokens: Seq[Token]): ParsingResult = {
    tokens match {
      case (_: PrintToken)::(_: LeftParenToken)::rest => expr(rest) match {
        case Success(expr: Expr, (_: RightParenToken)::rest) => Success(PrintStmt(expr), rest)
        case failure => failure
      }
      case _ => throw PanicException(0, s"unable to parse printstmt")    }

  }

  private def expr(tokens: Seq[Token]): ParsingResult = {
    equality(tokens)
  }

  private def equality(tokens: Seq[Token]): ParsingResult = {
    comparison(tokens) match {
      case Success(left: Expr, (op: (EqualEqualToken | BangEqualToken))::rest) =>
        equality(rest) match {
          case Success(right: Expr, afterBinary) => Success(Binary(left, op, right), afterBinary)
          case failure => failure
        }
      case other => other
    }
  }

  private def comparison(tokens: Seq[Token]): ParsingResult = {
    term(tokens) match {
      case Success(left: Expr, (op: (GreaterToken | GreaterEqualToken | LessToken | LessEqualToken ))::rest) =>
        comparison(rest) match {
          case Success(right: Expr, afterBinary) => Success(Binary(left, op, right), afterBinary)
          case failure => failure
        }
      case other => other
    }
  }

  private def term(tokens: Seq[Token]): ParsingResult = {
    factor(tokens) match {
      case Success(left: Expr, (op: (PlusToken | MinusToken))::rest) =>
        term(rest) match {
          case Success(right: Expr, afterBinary) => Success(Binary(left, op, right), afterBinary)
          case failure => failure
        }
      case other => other
    }
  }

  private def factor(tokens: Seq[Token]): ParsingResult = {
    unary(tokens) match {
      case Success(left: Expr, (op: (StarToken | SlashToken))::rest) =>
        factor(rest) match {
          case Success(right: Expr, afterBinary) => Success(Binary(left, op, right), afterBinary)
          case failure => failure
        }
      case other => other
    }
  }

  private def unary(tokens: Seq[Token]): ParsingResult = {
    primary(tokens) match {
      case s: Success => s
      case _ => tokens match {
        case (op: UnaryOperator)::rest =>
          primary(rest) match {
            case Success(operand: Expr, afterOperand) => Success(Unary(op, operand), afterOperand)
            case failure => failure
          }
        case failure => Failure("expected unary operator")
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
          case Success(expr: Expr, (_: RightParenToken)::afterGrouping) =>
              Success(Grouping(expr), afterGrouping)
          case _ => Failure("unterminated grouping")
        }
      case unexpected => Failure(s"expected grouping, got $unexpected")
    }
  }
}
