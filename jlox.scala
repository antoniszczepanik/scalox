object Jlox {

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

    def runFile(fileName: String) = {
        val lines = scala.io.Source.fromFile(fileName).mkString
        if (!run(lines)) then System.exit(1)
    }

    def run(command: String): Boolean = {
        Scanner.scanTokens(command) match {
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

case class Token(tokenType: TokenType.Value, lexeme: String, literal: Option[String|Int], line: Int) {
    override def toString =
        literal match {
            case Some(x) => f"${tokenType.toString()} ${lexeme} ${x}"
            case None    => f"${tokenType.toString()} ${lexeme}"
        }
}

object Scanner {
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

    def getToken(source: String): (Option[Token], String) = {
        val (first, rest) = (source(0), source.substring(1))
        first match {
            // Single character tokens.
            case '('   => (Some(newToken(TokenType.LEFT_PAREN)), rest)
            case ')'   => (Some(newToken(TokenType.RIGHT_PAREN)), rest)
            case '{'   => (Some(newToken(TokenType.LEFT_BRACE)), rest)
            case '}'   => (Some(newToken(TokenType.RIGHT_BRACE)), rest)
            case ','   => (Some(newToken(TokenType.COMMA)), rest)
            case '.'   => (Some(newToken(TokenType.DOT)), rest)
            case '-'   => (Some(newToken(TokenType.MINUS)), rest)
            case '+'   => (Some(newToken(TokenType.PLUS)), rest)
            case ';'   => (Some(newToken(TokenType.SEMICOLON)), rest)
            case '*'   => (Some(newToken(TokenType.STAR)), rest)
 
            // Double character tokens.
            case '!'   => getNextToken(rest, doesFirstEqual(rest, '='), TokenType.BANG_EQUAL, TokenType.BANG)
            case '='   => getNextToken(rest, doesFirstEqual(rest, '='), TokenType.EQUAL_EQUAL, TokenType.EQUAL)
            case '<'   => getNextToken(rest, doesFirstEqual(rest, '='), TokenType.LESS_EQUAL, TokenType.LESS)
            case '>'   => getNextToken(rest, doesFirstEqual(rest, '='), TokenType.GREATER_EQUAL, TokenType.GREATER)

            // Ignored.
            case '\n' | '\r' | '\t' | ' ' => (None, rest)

            case '"'   => getStringToken(rest)
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => getNumberToken(rest)

            case other => throw new Exception(s"Illegal character: '$other'")
        }
    }

    def newToken(tt: TokenType.Value) = Token(tt, "lexeme", None, 0)
    def getNextToken(source: String, cond: Boolean, _then: TokenType.Value, _else: TokenType.Value) = {
        if cond then
            (Some(newToken(_then)), source.substring(1))
        else 
            (Some(newToken(_else)), source)
    }
    def doesFirstEqual(str: String, char: Char): Boolean = (str.size > 0 && str(0) == char)

    def getStringToken(rest: String) = {
        // TODO: Consume until closing " or error.
    }
    def getNumberToken(rest: String) = {
        // TODO: Consume until not a number or error.
    }
}
