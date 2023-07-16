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

    private def run(command: String): Unit = {
        try {
          new Scanner().scanTokens(command)
            .map(Parser.parse)
            .map(Interpreter.interpret)
            .foreach(literalVal => println(s"result: ${literalVal.value}"))
        } catch {
            case PanicException(line, msg) => panic(line, msg)
            case unexpectedException => throw unexpectedException
        }
    }

    def panic(line: Int, msg: String) = if isInteractive then printError(line, msg) else printErrorAndExit(line, msg)

    private def printError(line: Int, msg: String) = println(s"[line $line] ERROR: $msg")

    private def printErrorAndExit(line: Int, msg: String) = {
        printError(line, msg)
        System.exit(1)
    }
}

case class PanicException(line: Int, msg: String) extends Exception(msg)
