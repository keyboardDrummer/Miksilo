package core.particles

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import core.bigrammar.Labelled
import core.particles.exceptions.ParticleDependencyViolation
import core.particles.grammars.ProgramGrammar
import core.particles.node.Node

import scala.reflect.io.{Directory, File}

class CompilerFromParticles(val particles: Seq[Particle]) {

  validateDependencies(particles)

  def getGrammar: Labelled = {
    val state = buildState
    state.grammarCatalogue.find(ProgramGrammar)
  }

  def parseAndTransform(input: File): CompilationState = {
    val inputStream = File(input).slurp()
    val state: CompilationState = parseAndTransform(input.inputStream())
    state
  }

  def compile(input: File, outputDirectory: Directory): CompilationState = {
    val state: CompilationState = parseAndTransform(input.inputStream())

    PrintByteCodeToOutputDirectory.perform(input, outputDirectory, state)
    state
  }

  def transform(program: Node): Node = {
    val state: CompilationState = transformReturnState(program)
    state.program
  }

  def transformReturnState(program: Node): CompilationState = {
    val state = buildState
    state.program = program
    state.runPhases()
    state
  }

  def parse(input: String): Node = {
    val state = buildState
    state.program = state.parse(stringToInputStream(input))
    state.program
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def parseAndTransform(input: String): CompilationState = {
    parseAndTransform(stringToInputStream(input))
  }

  def parseAndTransform(input: InputStream): CompilationState = {
    val state = buildState
    state.program = state.parse(input)
    state.runPhases()
    state
  }

  def buildState: CompilationState = {
    val state = new CompilationState()
    for(particle <- particles.reverse)
    {
      particle.inject(state)
    }
    state
  }

  //Bad order error
  //All missing dependencies.
  def validateDependencies(transformations: Seq[Particle]) = {
    var available = Set.empty[Contract]
    for (transformation <- transformations.reverse) {
      transformation.dependencies.foreach(dependency =>
        if (!available.contains(dependency))
          throw new ParticleDependencyViolation(dependency, transformation)
      )
      available += transformation
    }
  }

  def replace(marker: Particle, splice: Seq[Particle]): Seq[Particle] = {
    val pivot = particles.indexWhere(particle => marker == particle)
    val (before,after) = particles.splitAt(pivot)
    before ++ splice ++ after.drop(1)
  }
}
