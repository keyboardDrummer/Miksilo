package core.deltas

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import core.deltas.exceptions.DeltaDependencyViolation
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.Node

import scala.collection.mutable
import scala.reflect.io.{Directory, File}

case class Phase(key: Delta, action: Compilation => Unit)

object Language {

  def replace(deltas: Seq[Delta], marker: Delta, splice: Seq[Delta]): Seq[Delta] = {
    val pivot = deltas.indexWhere(particle => marker == particle)
    val (before,after) = deltas.splitAt(pivot)
    before ++ splice ++ after.drop(1)
  }

  def spliceBeforeTransformations(deltas: Seq[Delta], implicits: Seq[Delta], splice: Seq[Delta]): Seq[Delta] = {
    val implicitsSet = implicits.toSet
    deltas.filter(t => !implicitsSet.contains(t)) ++ splice ++ implicits
  }

  def spliceAfterTransformations(deltas: Seq[Delta], implicits: Seq[Delta], splice: Seq[Delta]): Seq[Delta] = {
    val implicitsSet = implicits.toSet
    implicits ++ splice ++ deltas.filter(t => !implicitsSet.contains(t))
  }

}

class Language(val deltas: Seq[Delta]) {
  validateDependencies(deltas)

  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammars = new LanguageGrammars

  var compilerPhases: List[Phase] = List.empty
  var _parse: InputStream => Node = _

  for(particle <- deltas.reverse)
  {
    particle.inject(this)
  }

  def parseAndTransform(input: File): Compilation = {
    parseAndTransform(input.inputStream())
  }

  def compile(input: File, outputDirectory: Directory): Compilation = {
    val state: Compilation = parseAndTransform(input.inputStream())

    PrintByteCodeToOutputDirectory.perform(input, outputDirectory, state)
    state
  }

  def transform(program: Node): Compilation = {
    val state = new Compilation(this)
    state.program = program
    state.runPhases()
    state
  }

  def parse(input: InputStream): Node = {
    val compilation = new Compilation(this)
    compilation.program = _parse(input)
    compilation.program
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def parseAndTransform(input: InputStream): Compilation = {
    val compilation = new Compilation(this)
    compilation.program = _parse(input)
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
