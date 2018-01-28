package util

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.Node
import core.language.Language

import scala.reflect.io.File

class TestingLanguage(val deltas: Seq[Delta], compilerName: String) {
  val statistics = new Statistics(TestLanguageBuilder.statistics)

  lazy val language: Language = buildLanguage
  lazy val grammars: LanguageGrammars = language.grammars

  def parseAndTransform(input: String): Compilation = {
    parseAndTransform(new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8)))
  }

  def parseAndTransform(input: File): Compilation = {
    parseAndTransform(input.inputStream())
  }

  def compile(inputStream: InputStream, outputFile: File): Compilation = {
    val state: Compilation = parseAndTransform(inputStream)

    PrintByteCodeToOutputDirectory.perform(outputFile, state)
    state
  }

  def transform(program: Node): Compilation = {
    val state = new Compilation(language)
    state.program = program
    runPhases(state)
    state
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

  private def runPhases(state: Compilation): Unit = {
    statistics.profile("running phases", {
      for(phase <- language.compilerPhases)
        statistics.profile("run " + phase.key.description, phase.action(state))
    })
  }

  def parse(input: InputStream): Node = {
    val state = new Compilation(language)
    justParse(input, state)
    state.program
  }

  private def justParse(input: InputStream, state: Compilation): Unit = {
    statistics.profile("parse", state.program = language.parse(input).get)
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def parseAndTransform(input: InputStream): Compilation = {
    val state = new Compilation(language)
    justParse(input, state)
    runPhases(state)
    state
  }

  class WrappedContract(contract: Contract) extends Contract {

    override def name: String = contract.name

    override def toString: String = contract.toString

    override def dependencies: Set[Contract] = contract.dependencies.map(dependency => new WrappedContract(dependency))

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
      new Language(Seq(new Delta {
        override def description: String = "Instrument buildParser"

        override def inject(language: Language): Unit = {
          val old = language.buildParser
          language.buildParser = () => {
            statistics.profile("build parser", old())
          }
          super.inject(language)
        }
      }) ++ deltas.map(delta => new WrappedDelta(delta)))
    })
  }
}
