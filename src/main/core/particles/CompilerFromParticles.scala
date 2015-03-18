package core.particles

import core.biGrammar.Labelled
import core.exceptions.ParticleDependencyViolation
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
    val state: CompilationState = parseAndTransform(inputStream)
    state
  }

  def compile(input: File, outputDirectory: Directory): CompilationState = {
    val inputStream = File(input).slurp()
    val state: CompilationState = parseAndTransform(inputStream)

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
    state.parseString(input)
    state.program
  }

  def parseAndTransform(input: String): CompilationState = {
    val state = buildState
    state.parseString(input)
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
