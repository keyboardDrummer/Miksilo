package core.particles

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import core.bigrammar.Labelled
import core.particles.exceptions.ParticleDependencyViolation
import core.particles.node.Node

import scala.reflect.io.{Directory, File}

class CompilerFromDeltas(val deltas: Seq[Delta]) {

  validateDependencies(deltas)
  lazy val language: Language = buildLanguage

  def getGrammar: Labelled = {
    language.grammarCatalogue.root
  }

  def parseAndTransform(input: File): Compilation = {
    val state: Compilation = parseAndTransform(input.inputStream())
    state
  }

  def compile(input: File, outputDirectory: Directory): Compilation = {
    val state: Compilation = parseAndTransform(input.inputStream())

    PrintByteCodeToOutputDirectory.perform(input, outputDirectory, state)
    state
  }

  def transform(program: Node): Compilation = {
    val state = new Compilation(language)
    state.program = program
    state.runPhases()
    state
  }

  def parse(input: InputStream): Node = {
    val state = new Compilation(language)
    state.program = language.parse(input)
    state.program
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def parseAndTransform(input: InputStream): Compilation = {
    val state = new Compilation(language)
    state.program = language.parse(input)
    state.runPhases()
    state
  }

  def buildLanguage: Language = {
    val result = new Language()
    for(particle <- deltas.reverse)
    {
      particle.inject(result)
    }
    result
  }

  //Bad order error
  //All missing dependencies.
  def validateDependencies(transformations: Seq[Delta]): Unit = {
    var available = Set.empty[Contract]
    for (transformation <- transformations.reverse) {
      transformation.dependencies.foreach(dependency =>
        if (!available.contains(dependency))
          throw ParticleDependencyViolation(dependency, transformation)
      )
      available += transformation
    }
  }

  def replace(marker: Delta, splice: Seq[Delta]): Seq[Delta] = {
    val pivot = deltas.indexWhere(particle => marker == particle)
    val (before,after) = deltas.splitAt(pivot)
    before ++ splice ++ after.drop(1)
  }

  def spliceBeforeTransformations(implicits: Seq[Delta], splice: Seq[Delta]): Seq[Delta] = {
    val implicitsSet = implicits.toSet
    deltas.filter(t => !implicitsSet.contains(t)) ++ splice ++ implicits
  }

  def spliceAfterTransformations(implicits: Seq[Delta], splice: Seq[Delta]): Seq[Delta] = {
    val implicitsSet = implicits.toSet
    implicits ++ splice ++ deltas.filter(t => !implicitsSet.contains(t))
  }
}
