package core.language

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import core.deltas._
import core.deltas.exceptions.{BadInputException, DeltaDependencyViolation}
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.Node
import core.smarts.ConstraintBuilder

import scala.collection.mutable
import scala.reflect.io.File
import scala.util.Try

class Language(val deltas: Seq[Delta]) {
  validateDependencies(deltas)

  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammars = new LanguageGrammars
  var capabilities: Seq[Capability] = Seq.empty
  var compilerPhases: List[Phase] = List.empty
  var buildParser: () => (InputStream => Try[Node]) = () => null
  var collectConstraints: (Compilation, ConstraintBuilder) => Unit = _

  for(particle <- deltas.reverse)
  {
    particle.inject(this)
  }

  val parser = buildParser()

  def parse(input: InputStream): Try[Node] = parser(input)

  def parseAndTransform(input: File): Compilation = {
    parseAndTransform(input.inputStream())
  }

  def compile(inputStream: InputStream, outputFile: File): Compilation = {
    val state: Compilation = parseAndTransform(inputStream)

    PrintByteCodeToOutputDirectory.perform(outputFile, state)
    state
  }

  def transform(program: Node): Compilation = {
    val state = new Compilation(this)
    state.program = program
    state.runPhases()
    state
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def parseAndTransform(input: InputStream): Compilation = {
    val compilation = new Compilation(this)
    compilation.program = parse(input).get
    compilation.runPhases()
    compilation
  }

  //Bad order error
  //All missing dependencies.
  def validateDependencies(deltas: Seq[Delta]): Unit = {
    var available = Set.empty[Contract]
    for (delta <- deltas.reverse) {
      delta.dependencies.foreach(dependency =>
        if (!available.contains(dependency))
          throw DeltaDependencyViolation(dependency, delta)
      )
      available += delta
    }
  }
}

object Language {

  def replace(deltas: Seq[Delta], marker: Delta, splice: Seq[Delta]): Seq[Delta] = {
    val pivot = deltas.indexWhere(particle => marker == particle)
    val (before,after) = deltas.splitAt(pivot)
    before ++ splice ++ after.drop(1)
  }

  def spliceAndFilterTop(top: Seq[Delta], bottom: Seq[Delta], splice: Seq[Delta] = Seq.empty): Seq[Delta] = {
    val implicitsSet = bottom.toSet
    top.filter(t => !implicitsSet.contains(t)) ++ splice ++ bottom
  }

  def spliceAndFilterBottom(top: Seq[Delta], bottom: Seq[Delta], splice: Seq[Delta] = Seq.empty): Seq[Delta] = {
    val implicitsSet = top.toSet
    top ++ splice ++ bottom.filter(t => !implicitsSet.contains(t))
  }

}

case class Phase(key: Delta, action: Compilation => Unit)

case class ParseException(message: String) extends BadInputException {
  override def toString: String = message
}

object NoSourceException extends BadInputException

