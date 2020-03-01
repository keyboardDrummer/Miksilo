package core.language

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import core.LazyLogging
import core.language.exceptions.BadInputException
import core.parsers.editorParsers.{StopFunction, TimeRatioStopFunction}
import core.parsers.sequences.SingleResultParser
import core.parsers.strings.StringParserWriter
import core.smarts.{ConstraintBuilder, CouldNotApplyConstraints, Factory, SolveException}

import scala.collection.mutable
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
      val afterConstraintCollection = System.currentTimeMillis()
      val collectDuration = afterConstraintCollection - start
      compilation.metrics.measure("Constraint collection time", collectDuration)

      solver.run() match {
        case Success(_) =>
          compilation.remainingConstraints = Seq.empty
        case Failure(e:CouldNotApplyConstraints) =>
          compilation.remainingConstraints = e.constraints
        case Failure(e:SolveException) =>
          throw ConstraintException(e)
        case Failure(e) => throw e
      }
      val solveDuration = System.currentTimeMillis() - afterConstraintCollection
      compilation.metrics.measure("Constraint solving time", solveDuration)
      compilation.proofs = solver.proofs
      if (compilation.remainingConstraints.nonEmpty) {
        compilation.stopped = true
      }
      compilation.diagnostics ++= compilation.remainingConstraints.flatMap(
        constraint => constraint.getDiagnostic(compilation))
    })
  }

  def getParsePhaseFromParser[Program](parserWriter: StringParserWriter)(
    getSourceElement: (Program, String) => SourceElement,
    parserBuilder: parserWriter.Parser[Program],
    stopFunction: StopFunction = new TimeRatioStopFunction): Phase = {

    val builtParser = new CompilationState[mutable.HashMap[String, SingleResultParser[Program, parserWriter.Input]]](_ => mutable.HashMap.empty)

    Phase("parse", "parse the source code", compilation => {
      val uri = compilation.rootFile.get
      val input = compilation.fileSystem.getFile(uri)

      val parsers = builtParser(compilation)

      def createParser(): SingleResultParser[Program, parserWriter.Input] = {
        val parseText = compilation.fileSystem.getFileParseText(uri)
        val result = parserWriter.SequenceParserExtensions[Program](parserBuilder).getSingleResultParser(parseText)
        compilation.fileSystem.setTextChangedHandler(uri, new TextChangeHandler {
          override def handleChange(from: Int, until: Int, text: String): Unit = {
            result.changeRange(from, until, text.length)
          }
        })
        result
      }

      val parsersPerFile = parsers.getOrElseUpdate(uri, createParser())

      val parseResult = parsersPerFile.parse(input, stopFunction, compilation.metrics)
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

  def compileAst(program: SourceElement): Compilation = {
    val compilation = Compilation.fromAst(this, program)
    compilation.program = program
    compilation.runPhases()
    compilation
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def compileString(input: String): Compilation = {
    val compilation = Compilation.singleFile(this, input)
    compilation.runPhases()
    compilation
  }

  def compile(): Compilation = {
    val compilation = new Compilation(this, EmptyFileSystem, None)
    compilation.runPhases()
    compilation
  }
}

case class Phase(key: Any, description: String, action: Compilation => Unit) {}

case class ParseException(message: String) extends BadInputException {
  override def toString: String = message
}

object NoSourceException extends BadInputException
