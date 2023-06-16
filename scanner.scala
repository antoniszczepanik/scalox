package scalox

class Scanner {

    private var line: Int = 1

    def scanTokens(source: String): Option[List[Token]]  = {
        getToken(source) match {
            case (Some(token), rest) => {
                scanTokens(rest) match {
                    case Some(tokens) => Some(token :: tokens)
                    case None => Some(List(token))
                }
            }
            case (None, rest) if rest.size == 0 => None
            case (None, rest) => scanTokens(rest)
        }
    }

    private def getToken(source: String): (Option[Token], String) = {
       source.toList match {
            case List() => (None, "")
            // Single character tokens.
            case '('::rest => (Some(LeftParenToken(line)), rest.mkString)
            case ')'::rest => (Some(RightParenToken(line)), rest.mkString)
            case '{'::rest => (Some(LeftBraceToken(line)), rest.mkString)
            case '}'::rest => (Some(RightBraceToken(line)), rest.mkString)
            case ','::rest => (Some(CommaToken(line)), rest.mkString)
            case '.'::rest => (Some(DotToken(line)), rest.mkString)
            case '-'::rest => (Some(MinusToken(line)), rest.mkString)
            case '+'::rest => (Some(PlusToken(line)), rest.mkString)
            case ';'::rest => (Some(SemicolonToken(line)), rest.mkString)
            case '*'::rest => (Some(StarToken(line)), rest.mkString)
 
            // Double character tokens.
            case '!'::'='::rest => (Some(BangEqualToken(line)), rest.mkString)
            case '!'::rest      => (Some(BangToken(line)), rest.mkString)
            case '='::'='::rest => (Some(EqualEqualToken(line)), rest.mkString)
            case '='::rest      => (Some(EqualToken(line)), rest.mkString)
            case '<'::'='::rest => (Some(LessEqualToken(line)), rest.mkString)
            case '<'::rest      => (Some(LessToken(line)), rest.mkString)
            case '>'::'='::rest => (Some(GreaterEqualToken(line)), rest.mkString)
            case '>'::rest      => (Some(GreaterToken(line)), rest.mkString)
            // Ignore a comment.
            case '/'::'/'::rest => (None, rest.dropWhile{ _ != '\n' }.mkString)
            case '/'::rest      => (Some(SlashToken(line)), rest.mkString)

            // Ignored.
            case '\n'::rest => line+=1; (None, rest.mkString)
            case '\r'::rest => (None, rest.mkString)
            case '\t'::rest => (None, rest.mkString)
            case  ' '::rest => (None, rest.mkString)

            // Strings and digits.
            case '"'::rest         => getStringToken(rest.mkString)
            case s if s(0).isDigit => getNumberToken(s.mkString)
            
            // Identifiers and keywords.
            case s if s(0).isLetterOrDigit || s(0) == '_' => getIdentifierOrKeyword(s.mkString)

            case unexpected::_ => Scalox.error(line, f"Illegal character: '${unexpected}'"); (None, "")
        }
    }

    private def getStringToken(rest: String): (Option[Token], String) = {
        def consumeChars(acc: String, rest: String): (String, String) = {
            rest.toList match {
                case List() | '\n'::_ => Scalox.error(line, f"Unterminated string"); ("", "")
                case '"'::rest        => (acc, rest.mkString)
                case other::rest      => consumeChars(acc + other, rest.mkString)
            }
        }
        val (stringVal, left) = consumeChars("", rest)
        (Some(StringToken(line, stringVal)), left)
    }

    private def getNumberToken(rest: String) = {
        def consumeDigits(acc: String, rest: String): (String, String) = {
            rest.toList match {
                case first::rest if first.isDigit || first == '.' => consumeDigits(acc + first, rest.mkString)
                case rest                         => (acc, rest.mkString)
            }
        }
        
        val (digitVal, left) = consumeDigits("", rest)
        (Some(NumberToken(line, digitVal.toDouble)), left)
    }

    private def getIdentifierOrKeyword(source: String): (Option[Token], String)  = {
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
            case "class" => (Some(ClassToken(line)), rest)
            case "else"  => (Some(ElseToken(line)), rest)
            case "false" => (Some(FalseToken(line)), rest)
            case "fun"   => (Some(FunToken(line)), rest)
            case "for"   => (Some(ForToken(line)), rest)
            case "if"    => (Some(IfToken(line)), rest)
            case "nil"   => (Some(NilToken(line)), rest)
            case "or"    => (Some(OrToken(line)), rest)
            case "print" => (Some(PrintToken(line)), rest)
            case "return"=> (Some(ReturnToken(line)), rest)
            case "super" => (Some(SuperToken(line)), rest)
            case "this"  => (Some(ThisToken(line)), rest)
            case "true"  => (Some(TrueToken(line)), rest)
            case "var"   => (Some(VarToken(line)), rest)
            case "while" => (Some(WhileToken(line)), rest)
            // Identifier.
            case _       => (Some(IdentifierToken(line, unidentified)), rest)
        }
    }
}

abstract class Token {
    override def toString = getClass.getSimpleName
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
    override def toString = f"${getClass.getSimpleName} ${value}"
}
case class StringToken(line: Int, value: String)     extends Token {
    override def toString = f"${getClass.getSimpleName} '${value}'"
}
case class NumberToken(line: Int, value: Double)     extends Token {
    override def toString = f"${getClass.getSimpleName} ${value}"
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
case class WhileToken(line: Int)  extends Token
case class EofToken(line: Int)    extends Token
