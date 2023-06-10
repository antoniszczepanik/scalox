package scalox

import scala.util.{Try, Success, Failure}

object Scalox {

    private var isInteractive = false

    def main(args: Array[String]) = {
        args.size match {
            case 0 => isInteractive = true; prompt
            case 1 => runFile(args(0))
            case _ => {
                println("Usage: scalox [script]")
                System.exit(1)
            }
        }
    }

    private def prompt: Unit = {
        print("> ")
        val line = scala.io.StdIn.readLine
        if (line == null) return
        run(line)
        prompt
    }

    private def runFile(path: String) = {
        readFile(path) match {
            case Success(content) => run(content)
            case Failure(msg)     => println(f"ERROR: ${msg}")
        }
    }

    private def readFile(path: String): Try[String] = Try(io.Source.fromFile(path).mkString)

    private def run(command: String) = {
        new Scanner().scanTokens(command) match {
            case Some(tokens) => println(Interpreter.interpret(Parser.parse(tokens)))
            case None         => ()
        }
    }

    def error(line: Int, msg: String) = if isInteractive then errorAndContinue(line, msg) else errorAndExit(line, msg)

    private def errorAndContinue(line: Int, msg: String) = println(s"[line $line] ERROR: $msg")

    private def errorAndExit(line: Int, msg: String) = {
        errorAndContinue(line, msg)
        System.exit(1)
    }
}
