package scalox

import scala.util.{Try, Success, Failure}

class Scanner {

    private[this] var line: Int = 1

    def scanTokens(source: String): Option[List[Token]]  = {
        if (source.size == 0) then None else
        getToken(source) match {
            case (None, rest) => scanTokens(rest)
            case (Some(token), rest) => {
                scanTokens(rest) match {
                    case None => Some(List(token))
                    case Some(tokens) => Some(token :: tokens)
                }
            }
        }
    }

    private[this] def getToken(source: String): (Option[Token], String) = {
       source.toList match {
            case List() => (None, "")
            // Single character tokens.
            case '('::rest => (Some(LeftParenToken(this.line)), rest.mkString)
            case ')'::rest => (Some(RightParenToken(this.line)), rest.mkString)
            case '{'::rest => (Some(LeftBraceToken(this.line)), rest.mkString)
            case '}'::rest => (Some(RightBraceToken(this.line)), rest.mkString)
            case ','::rest => (Some(CommaToken(this.line)), rest.mkString)
            case '.'::rest => (Some(DotToken(this.line)), rest.mkString)
            case '-'::rest => (Some(MinusToken(this.line)), rest.mkString)
            case '+'::rest => (Some(PlusToken(this.line)), rest.mkString)
            case ';'::rest => (Some(SemicolonToken(this.line)), rest.mkString)
            case '*'::rest => (Some(StarToken(this.line)), rest.mkString)
 
            // Double character tokens.
            case '!'::'='::rest => (Some(BangEqualToken(this.line)), rest.mkString)
            case '!'::rest      => (Some(BangToken(this.line)), rest.mkString)
            case '='::'='::rest => (Some(EqualEqualToken(this.line)), rest.mkString)
            case '='::rest      => (Some(EqualToken(this.line)), rest.mkString)
            case '<'::'='::rest => (Some(LessEqualToken(this.line)), rest.mkString)
            case '<'::rest      => (Some(LessToken(this.line)), rest.mkString)
            case '>'::'='::rest => (Some(GreaterEqualToken(this.line)), rest.mkString)
            case '>'::rest      => (Some(GreaterToken(this.line)), rest.mkString)
            // Ignore a comment.
            case '/'::'/'::rest => (None, rest.dropWhile{ _ != '\n' }.mkString)
            case '/'::rest      => (Some(SlashToken(this.line)), rest.mkString)

            // Ignored.
            case '\n'::rest => this.line+=1; (None, rest.mkString)
            case '\r'::rest => (None, rest.mkString)
            case '\t'::rest => (None, rest.mkString)
            case  ' '::rest => (None, rest.mkString)

            // Strings and digits.
            case '"'::rest         => getStringToken(rest.mkString)
            case s if s(0).isDigit => getNumberToken(s.mkString)
            
            // Identifiers and keywords.
            case s if s(0).isLetterOrDigit || s(0) == '_' => getIdentifierOrKeyword(s.mkString)

            case unexpected::_ => Scalox.error(this.line, f"Illegal character: '${unexpected}'"); (None, "")
        }
    }

    private[this] def getStringToken(rest: String): (Option[Token], String) = {
        def consumeChars(acc: String, rest: String): (String, String) = {
            rest.toList match {
                case List() | '\n'::_ => Scalox.error(this.line, f"Unterminated string"); ("", "")
                case '"'::rest        => (acc, rest.mkString)
                case other::rest      => consumeChars(acc + other, rest.mkString)
            }
        }
        val (stringVal, left) = consumeChars("", rest)
        (Some(StringToken(this.line, stringVal)), left)
    }

    private[this] def getNumberToken(rest: String) = {
        def consumeDigits(acc: String, rest: String): (String, String) = {
            rest.toList match {
                case first::rest if first.isDigit || first == '.' => consumeDigits(acc + first, rest.mkString)
                case rest                         => (acc, rest.mkString)
            }
        }
        
        val (digitVal, left) = consumeDigits("", rest)
        (Some(NumberToken(this.line, digitVal.toDouble)), left)
    }

    private[this] def getIdentifierOrKeyword(source: String): (Option[Token], String)  = {
        def takeAlphanum(acc: String, rest: String): (String, String) = {
            rest.toList match {
                case List()                                            => (acc, "")
                case char::rest if char.isLetterOrDigit || char == '_' => takeAlphanum(acc + char, rest.mkString)
                case _                                                 => (acc, rest)
            }
        }

        val (unidentified, rest) = takeAlphanum("", source)
        unidentified match {
            // Keywords.
            case "class" => (Some(ClassToken(this.line)), rest)
            case "else"  => (Some(ElseToken(this.line)), rest)
            case "false" => (Some(FalseToken(this.line)), rest)
            case "fun"   => (Some(FunToken(this.line)), rest)
            case "for"   => (Some(ForToken(this.line)), rest)
            case "if"    => (Some(IfToken(this.line)), rest)
            case "nil"   => (Some(NilToken(this.line)), rest)
            case "or"    => (Some(OrToken(this.line)), rest)
            case "print" => (Some(PrintToken(this.line)), rest)
            case "return"=> (Some(ReturnToken(this.line)), rest)
            case "super" => (Some(SuperToken(this.line)), rest)
            case "this"  => (Some(ThisToken(this.line)), rest)
            case "true"  => (Some(TrueToken(this.line)), rest)
            case "var"   => (Some(VarToken(this.line)), rest)
            case "while" => (Some(WhileToken(this.line)), rest)
            // Identifier.
            case _       => (Some(IdentifierToken(this.line, unidentified)), rest)
        }
    }
}

abstract class Token {
    override def toString = this.getClass.getSimpleName
}
case class LeftParenToken(line: Int)  extends Token
case class RightParenToken(line: Int) extends Token
case class LeftBraceToken(line: Int)  extends Token
case class RightBraceToken(line: Int) extends Token
case class CommaToken(line: Int)      extends Token
case class DotToken(line: Int)        extends Token
case class MinusToken(line: Int)      extends Token
case class PlusToken(line: Int)       extends Token
case class SemicolonToken(line: Int)  extends Token
case class SlashToken(line: Int)      extends Token
case class StarToken(line: Int)       extends Token
// One or two character tokens.
case class BangToken(line: Int)         extends Token
case class BangEqualToken(line: Int)    extends Token
case class EqualToken(line: Int)        extends Token
case class EqualEqualToken(line: Int)   extends Token
case class GreaterToken(line: Int)      extends Token
case class GreaterEqualToken(line: Int) extends Token
case class LessToken(line: Int)         extends Token
case class LessEqualToken(line: Int)    extends Token
// Literals.
case class IdentifierToken(line: Int, value: String) extends Token {
    override def toString = f"${this.getClass.getSimpleName} ${value}"
}
case class StringToken(line: Int, value: String)     extends Token {
    override def toString = f"${this.getClass.getSimpleName} ${value}"
}
case class NumberToken(line: Int, value: Double)     extends Token {
    override def toString = f"${this.getClass.getSimpleName} ${value}"
}
// Keywords.
case class AndToken(line: Int)    extends Token
case class ClassToken(line: Int)  extends Token
case class ElseToken(line: Int)   extends Token
case class FalseToken(line: Int)  extends Token
case class FunToken(line: Int)    extends Token
case class ForToken(line: Int)    extends Token
case class IfToken(line: Int)     extends Token
case class NilToken(line: Int)    extends Token
case class OrToken(line: Int)     extends Token
case class PrintToken(line: Int)  extends Token
case class ReturnToken(line: Int) extends Token
case class SuperToken(line: Int)  extends Token
case class ThisToken(line: Int)   extends Token
case class TrueToken(line: Int)   extends Token
case class VarToken(line: Int)    extends Token
case class WhileToken(line: Int) extends Token
case class EofToken(line: Int)    extends Token
