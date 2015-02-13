package core.transformation

import core.exceptions.ParticleDependencyViolation
import core.transformation.sillyCodePieces.Particle

import scala.reflect.io.{Directory, File}

class CompilerFromParticles(val particles: Seq[Particle]) extends Compiler {

  validateDependencies(particles)

  def parseAndTransform(input: File): TransformationState = {
    val inputStream = File(input).slurp()
    val state: TransformationState = parseAndTransform(inputStream)
    state
  }

  def compile(input: File, outputDirectory: Directory) {
    val inputStream = File(input).slurp()
    val state: TransformationState = parseAndTransform(inputStream)

    PrintByteCodeToOutputDirectory.perform(input, outputDirectory, state)
  }

  def transform(program: MetaObject): MetaObject = {
    val state = buildState
    state.program = program
    state.runPhases()
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

  def spliceBefore(beforeThese: Seq[Particle], splice: Seq[Particle]): Seq[Particle] = {
    val beforeSet = beforeThese.toSet
    particles.filter(particle => !beforeSet.contains(particle)) ++ splice ++ beforeThese
  }

  def spliceAfter(afterThese: Seq[Particle], splice: Seq[Particle]): Seq[Particle] = {
    val afterTheseSet = afterThese.toSet
    afterThese ++ splice ++ particles.filter(particle => !afterTheseSet.contains(particle))
  }
}
