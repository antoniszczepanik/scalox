import scala.util.{Try, Success, Failure}

object Scalox {

    def main(args: Array[String]) = {
        args.size match {
            case 0 => prompt
            case 1 => runFile(args(0))
            case _ => {
                println("Usage: jlox [script]")
                System.exit(1)
            }
        }
    }

    def prompt: Unit = {
        print("> ")
        val line = scala.io.StdIn.readLine
        if (line == null) return
        val _ = run(line)
        prompt
    }

    def runFile(path: String) = {
        val f = readFile(path)
        f match {
            case Success(content) => if !run(content) then System.exit(1)
            case Failure(msg)     => println(f"ERROR: ${msg}")
        }
    }

    def readFile(path: String): Try[String] = Try { io.Source.fromFile(path).mkString }

    def run(command: String): Boolean = {
        val s = new Scanner
        s.scanTokens(command) match {
            case None => false
            case Some(tokens) => {
                tokens map println
                true
            }
        }
    }

    def error(line: Int, message: String) = {
        println(s"[line $line] ERROR: $message")
    }
}

object TokenType extends Enumeration
{
    type Token = Value
    // Single-character tokens.
    val LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR = Value
    // One or two character tokens.
    val  BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL = Value
    // Literals.
    val IDENTIFIER, STRING, NUMBER = Value
    // Keywords.
    val AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE = Value
    val EOF = Value
}

case class Token(tokenType: TokenType.Value, lexeme: String, literal: Option[String|Double], line: Int) {
    override def toString =
        literal match {
            case Some(x) => f"${tokenType.toString()} ${lexeme} ${x}"
            case None    => f"${tokenType.toString()} ${lexeme}"
        }
}

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
            case '('::rest => (Some(newToken(TokenType.LEFT_PAREN)), rest.mkString)
            case ')'::rest => (Some(newToken(TokenType.RIGHT_PAREN)), rest.mkString)
            case '{'::rest => (Some(newToken(TokenType.LEFT_BRACE)), rest.mkString)
            case '}'::rest => (Some(newToken(TokenType.RIGHT_BRACE)), rest.mkString)
            case ','::rest => (Some(newToken(TokenType.COMMA)), rest.mkString)
            case '.'::rest => (Some(newToken(TokenType.DOT)), rest.mkString)
            case '-'::rest => (Some(newToken(TokenType.MINUS)), rest.mkString)
            case '+'::rest => (Some(newToken(TokenType.PLUS)), rest.mkString)
            case ';'::rest => (Some(newToken(TokenType.SEMICOLON)), rest.mkString)
            case '*'::rest => (Some(newToken(TokenType.STAR)), rest.mkString)
 
            // Double character tokens.
            case '!'::'='::rest => (Some(newToken(TokenType.BANG_EQUAL)), rest.mkString)
            case '!'::rest      => (Some(newToken(TokenType.BANG)), rest.mkString)
            case '='::'='::rest => (Some(newToken(TokenType.EQUAL_EQUAL)), rest.mkString)
            case '='::rest      => (Some(newToken(TokenType.EQUAL)), rest.mkString)
            case '<'::'='::rest => (Some(newToken(TokenType.LESS_EQUAL)), rest.mkString)
            case '<'::rest      => (Some(newToken(TokenType.LESS)), rest.mkString)
            case '>'::'='::rest => (Some(newToken(TokenType.GREATER_EQUAL)), rest.mkString)
            case '>'::rest      => (Some(newToken(TokenType.GREATER)), rest.mkString)
            // Ignore a comment.
            case '/'::'/'::rest => (None, rest.dropWhile{ _ != '\n' }.mkString)
            case '/'::rest      => (Some(newToken(TokenType.SLASH)),rest.mkString)

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

            case unexpected::_ => throw new Exception(f"Illegal character: '${unexpected}' in line ${this.line}")
        }
    }

    private[this] def newToken(tt: TokenType.Value) = Token(tt, "", None, this.line)

    private[this] def getStringToken(rest: String): (Option[Token], String) = {
        def consumeChars(acc: String, rest: String): (String, String) = {
            rest.toList match {
                case List() | '\n'::_ => throw new Exception(f"Unterminated string in line ${this.line}")
                case '"'::rest        => (acc, rest.mkString)
                case other::rest      => consumeChars(acc + other, rest.mkString)
            }
        }
        val (stringVal, left) = consumeChars("", rest)
        (Some(Token(TokenType.STRING, "", Some(stringVal), this.line)), left)
    }

    private[this] def getNumberToken(rest: String) = {
        def consumeDigits(acc: String, rest: String): (String, String) = {
            rest.toList match {
                case first::rest if first.isDigit || first == '.' => consumeDigits(acc + first, rest.mkString)
                case rest                         => (acc, rest.mkString)
            }
        }
        
        val (digitVal, left) = consumeDigits("", rest)
        (Some(Token(TokenType.NUMBER, "", Some(digitVal.toDouble), this.line)), left)
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
            case "class" => (Some(newToken(TokenType.CLASS)), rest)
            case "else"  => (Some(newToken(TokenType.ELSE)), rest)
            case "false" => (Some(newToken(TokenType.FALSE)), rest)
            case "fun"   => (Some(newToken(TokenType.FUN)), rest)
            case "for"   => (Some(newToken(TokenType.FOR)), rest)
            case "if"    => (Some(newToken(TokenType.IF)), rest)
            case "nil"   => (Some(newToken(TokenType.NIL)), rest)
            case "or"    => (Some(newToken(TokenType.OR)), rest)
            case "print" => (Some(newToken(TokenType.PRINT)), rest)
            case "return"=> (Some(newToken(TokenType.RETURN)), rest)
            case "super" => (Some(newToken(TokenType.SUPER)), rest)
            case "this"  => (Some(newToken(TokenType.THIS)), rest)
            case "true"  => (Some(newToken(TokenType.TRUE)), rest)
            case "var"   => (Some(newToken(TokenType.VAR)), rest)
            case "while" => (Some(newToken(TokenType.WHILE)), rest)
            // Identifier.
            case _       => (Some(Token(TokenType.IDENTIFIER, "", Some(unidentified), this.line)), rest)
        }

    }
}
