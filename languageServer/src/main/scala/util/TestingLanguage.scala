package util

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node.Node
import core.language.{Compilation, Language}

import scala.reflect.io.File

class TestingLanguage(val deltas: Seq[Delta], compilerName: String) {
  val statistics = new Statistics(TestLanguageBuilder.statistics)

  lazy val language: Language = buildLanguage
  lazy val grammars: LanguageGrammars = language.grammars

  def compile(input: String): Compilation = {
    compile(new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8)))
  }

  def compile(input: File): Compilation = {
    compile(input.inputStream())
  }

  def compileToFile(inputStream: InputStream, outputFile: File): Compilation = {
    val compilation: Compilation = compile(inputStream)

    PrintByteCodeToOutputDirectory.perform(outputFile, compilation)
    compilation
  }

  def compile(input: InputStream): Compilation = {
    val compilation = Compilation.singleFile(language, input)
    runPhases(compilation)
    compilation
  }

  def compileAst(program: Node): Compilation = {
    val compilation = Compilation.fromAst(language, program)
    runPhases(compilation)
    compilation
  }

  def spliceBeforeTransformations(implicits: Seq[Delta], splice: Seq[Delta]): Seq[Delta] = {
    val implicitsSet = implicits.toSet
    deltas.filter(t => !implicitsSet.contains(t)) ++ splice ++ implicits
  }

  def spliceAfterTransformations(implicits: Seq[Delta], splice: Seq[Delta]): Seq[Delta] = {
    val implicitsSet = implicits.toSet
    implicits ++ splice ++ deltas.filter(t => !implicitsSet.contains(t))
  }

  def replace(marker: Delta, splice: Seq[Delta]): Seq[Delta] = {
    val pivot = deltas.indexWhere(particle => marker == particle)
    val (before,after) = deltas.splitAt(pivot)
    before ++ splice ++ after.drop(1)
  }

  private def runPhases(compilation: Compilation): Unit = {
    statistics.profile("running phases", {
      for(phase <- language.compilerPhases) {
        statistics.profile("run " + phase.key.description, phase.action(compilation))
        if (compilation.stopped)
          return
      }
    })
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  class WrappedContract(contract: Contract) extends Contract {

    override def name: String = contract.name

    override def toString: String = contract.toString

    override def dependencies: Set[Contract] = contract.dependencies.map {
      case delta: Delta => new WrappedDelta(delta)
      case dependency => new WrappedContract(dependency)
    }

    override def equals(obj: scala.Any): Boolean = obj.equals(contract)

    override def hashCode(): Int = contract.hashCode()

    override def suffix: String = contract.suffix
  }

  class WrappedDelta(delta: Delta) extends WrappedContract(delta) with Delta {
    override def description: String = delta.description

    override lazy val toString: String = delta.toString

    override def inject(language: Language): Unit = statistics.profile("inject " + delta.name, delta.inject(language))

    override def equals(obj: scala.Any): Boolean = obj.equals(delta)

    override def hashCode(): Int = delta.hashCode()

    override def suffix: String = delta.suffix
  }

  def buildLanguage: Language = {
    statistics.profile("build language", {
      LanguageFromDeltas(deltas.map(delta => new WrappedDelta(delta)), false)
    })
  }
}
