package ru.spbau.maxim

import java.io.{PrintWriter}
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors

import ru.spbau.maxim.Parser.Command.StringArgs
import ru.spbau.maxim.Parser._

import scala.io.Source

/** Can evaluate command, pipeline
  * Can start REPL
 */
class Evaluator {
  private var continue = true
  private var currentDir: Path = Paths.get("").toAbsolutePath

  def createPath(file: String): Path = currentDir.resolve(file).normalize()

  /** Evaluates command
    */
  def evaluate(command: Command, stdIn: String): String = {
    def inputTexts(files: StringArgs): Seq[String] = {
      files match {
        case Nil => stdIn :: Nil
        case _ => files.map { file => Source.fromFile(createPath(file).toString).getLines().mkString("\n") }
      }
    }

    command match {
      case echo: Echo => echo.args.mkString(" ")

      case Pwd => currentDir.toString

      case Wc(args) => inputTexts(args).map { text =>
        val lines = text.split("\n").length
        val words = text.split("\\s").count(!_.isEmpty)
        s"$lines $words ${text.length}"
      }.mkString("\n")

      case Cat(files) => inputTexts(files).mkString("\n")

      case Exit =>
        continue = false
        "Exit"

      case Assignment(name, str) =>
        Model.putEnv(name, str)
        ""

      case Cd(dir) =>
        def tryCd(value: String): Path = {
          val newPath = createPath(value).toAbsolutePath
          if (!Files.exists(newPath)) {
            throw new IllegalArgumentException("provided dir does not exist")
          }
          newPath
        }
        currentDir = dir match {
          case Some(value) => tryCd(value)
          case None => currentDir
        }
        ""

      case Ls(dir) =>
        val dir_ = dir match {
          case Some(value) => createPath(value)
          case None => currentDir
        }
        Files.walk(dir_, 1)
          .filter(p => !p.equals(dir_))
          .map[String](p => p.getFileName.toString)
          .collect(Collectors.joining("\n"))

      case ExternalCommand(tokens) =>
        import scala.sys.process._
        var output = ""
        stringSeqToProcess(tokens).run(new ProcessIO(
          in => {
            val writer = new PrintWriter(in)
            writer.print(stdIn)
            writer.close()
          },
          out => {
            val src = Source.fromInputStream(out)
            output = src.getLines().mkString("\n")
            src.close()
          },
          _.close()
        ))
        output
    }
  }

  /** Evaluates pipeline
    * returns result of latest command
    */
  def evaluatePipeline(commands: Seq[Command]): String = {
    var lastOutput: String = ""
    for (command <- commands) {
      if (continue) {
        lastOutput = evaluate(command, lastOutput)
      }
    }
    lastOutput
  }

  /** Parses and evaluates pipeline
    * returns result of latest command or error message
    */
  def evaluatePipeline(commandStr: String): String = {
    try {
      val commands: Seq[Command] = CommandParser.parse(commandStr)
      try {
        evaluatePipeline(commands)
      } catch {
        case e: Exception => s"Error executing command:\n${e.getMessage}"
      }
    } catch {
      case e: Exception => "parse error"
    }
  }

  /** runs REPL
    */
  def loop(): Unit = {
    while (continue) {
      val commandStr: String = scala.io.StdIn.readLine()
      val output = evaluatePipeline(commandStr)
      println(output)
    }
  }
}
