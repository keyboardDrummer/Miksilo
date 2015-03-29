package transformations.bytecode.readJar

import core.particles.ParticleWithGrammar
import transformations.bytecode.attributes.UnParsedAttribute
import transformations.javac.JavaCompiler

object ClassFileSignatureDecompiler {

  val byteCodeParticles: Seq[ParticleWithGrammar] = Seq(UnParsedAttribute) ++ JavaCompiler.byteCodeTransformations
  def getDecompiler = {
    Seq(ParseAttributes) ++ Seq(UnParsedAttribute) ++ byteCodeParticles
  }
}
