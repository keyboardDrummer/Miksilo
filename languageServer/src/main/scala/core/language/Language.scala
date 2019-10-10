package core.language

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import core.language.exceptions.BadInputException
import core.parsers.editorParsers.{StopFunction, TimeRatioStopFunction}
import core.parsers.sequences.SingleResultParser
import core.parsers.strings.StringReaderLike
import core.smarts.{ConstraintBuilder, CouldNotApplyConstraints, Factory, SolveException}

import scala.collection.mutable
import scala.reflect.io.File
import scala.tools.nsc.interpreter.InputStream
import scala.util.{Failure, Success}

case class ConstraintException(solveException: SolveException) extends BadInputException {
  override def toString: String = "Could not solve semantic constraints:" + solveException.toString
}

object Language extends LazyLogging {
  def getConstraintPhase(collectConstraints: (Compilation, ConstraintBuilder) => Unit): Phase = {
    Phase("constraints", "solve constraints", compilation => {
      val factory = new Factory()
      val builder = new ConstraintBuilder(factory)

      val start = System.currentTimeMillis()
      collectConstraints(compilation, builder)
      val solver = builder.toSolver

      solver.run() match {
        case Success(_) =>
          compilation.remainingConstraints = Seq.empty
        case Failure(e:CouldNotApplyConstraints) =>
          compilation.remainingConstraints = e.constraints
        case Failure(e:SolveException) =>
          throw ConstraintException(e)
        case Failure(e) => throw e
      }
      logger.info(s"Constraint solving took ${System.currentTimeMillis() - start}ms")
      compilation.proofs = solver.proofs
      if (compilation.remainingConstraints.nonEmpty) {
        compilation.stopped = true
      }
      compilation.diagnostics ++= compilation.remainingConstraints.flatMap(
        constraint => constraint.getDiagnostic)
    })
  }

  def getParsePhaseFromParser[Program, Input <: StringReaderLike[Input]](
    getInput: InputStream => Input,
    getSourceElement: (Program, String) => SourceElement,
    parser: SingleResultParser[Program, Input],
    stopFunction: StopFunction = new TimeRatioStopFunction): Phase = {

    Phase("parse", "parse the source code", compilation => {
      val uri = compilation.rootFile.get
      val inputStream = compilation.fileSystem.getFile(uri)
      val time = System.currentTimeMillis()
      val parseResult = parser.parse(getInput(inputStream), stopFunction)
      val parseTime = System.currentTimeMillis() - time
      logger.info(s"Parsing took ${parseTime}ms")
      parseResult.resultOption.foreach(program => {
        compilation.program = getSourceElement(program, uri)
      })
      if (compilation.program == null) {
        compilation.stopped = true
      }
      if (!parseResult.successful) {
        val diagnostics = DiagnosticUtil.getDiagnosticsFromParseFailures(uri, parseResult.errors)
        compilation.addDiagnosticsWithFixes(diagnostics)
      }
    })

  }
}

class Language extends LazyLogging {

  val data: mutable.Map[Any, Any] = mutable.Map.empty
  var compilerPhases: List[Phase] = List.empty

  def insertPhaseAfter(insert: Phase, phaseKey: Any): Unit = {
    val index = compilerPhases.indexWhere(phase => phase.key == phaseKey)
    val (left, right) = compilerPhases.splitAt(index + 1)
    compilerPhases = left ++ (insert :: right)
  }

  //var extraCompileOptions: List[CustomCommand] = List.empty

  def compileString(input: String): Compilation = {
    compileStream(stringToInputStream(input))
  }

  def compileFile(input: File): Compilation = {
    compileStream(input.inputStream())
  }

  def compileAst(program: SourceElement): Compilation = {
    val compilation = Compilation.fromAst(this, program)
    compilation.program = program
    compilation.runPhases()
    compilation
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def compileStream(input: InputStream): Compilation = {
    val compilation = Compilation.singleFile(this, input)
    compilation.runPhases()
    compilation
  }
}

case class Phase(key: Any, description: String, action: Compilation => Unit) {}

case class ParseException(message: String) extends BadInputException {
  override def toString: String = message
}

object NoSourceException extends BadInputException
