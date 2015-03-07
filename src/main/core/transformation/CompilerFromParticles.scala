package core.transformation

import core.exceptions.ParticleDependencyViolation
import core.grammarDocument.Labelled
import core.transformation.grammars.ProgramGrammar
import core.transformation.sillyCodePieces.Particle

import scala.reflect.io.{Directory, File}

class CompilerFromParticles(val particles: Seq[Particle]) {

  validateDependencies(particles)

  def getGrammar: Labelled = {
    val state = buildState
    state.grammarCatalogue.find(ProgramGrammar)
  }

  def parseAndTransform(input: File): TransformationState = {
    val inputStream = File(input).slurp()
    val state: TransformationState = parseAndTransform(inputStream)
    state
  }

  def compile(input: File, outputDirectory: Directory): TransformationState = {
    val inputStream = File(input).slurp()
    val state: TransformationState = parseAndTransform(inputStream)

    PrintByteCodeToOutputDirectory.perform(input, outputDirectory, state)
    state
  }

  def transform(program: MetaObject): MetaObject = {
    val state: TransformationState = transformReturnState(program)
    state.program
  }

  def transformReturnState(program: MetaObject): TransformationState = {
    val state = buildState
    state.program = program
    state.runPhases()
    state
  }

  def parse(input: String): MetaObject = {
    val state = buildState
    state.parseString(input)
    state.program
  }

  def parseAndTransform(input: String): TransformationState = {
    val state = buildState
    state.parseString(input)
    state.runPhases()
    state
  }

  def buildState: TransformationState = {
    val state = new TransformationState()
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
