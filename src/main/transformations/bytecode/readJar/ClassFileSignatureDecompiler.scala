package transformations.bytecode.readJar

import core.particles.ParticleWithGrammar
import transformations.bytecode.attributes.{SignatureAttribute, ByteCodeAttribute, UnParsedAttribute}
import transformations.javac.JavaCompiler

object ClassFileSignatureDecompiler {

  val byteCodeParticles: Seq[ParticleWithGrammar] = Seq(UnParsedAttribute) ++ JavaCompiler.byteCodeWithoutInstructions
  val decompilerByteCodeParticles: Seq[ParticleWithGrammar] = byteCodeParticles.
    filter(particle => !particle.isInstanceOf[ByteCodeAttribute] || particle == SignatureAttribute)
  def getDecompiler = {
    Seq(ParseKnownAttributes, DecompileByteCodeSignature) ++ decompilerByteCodeParticles
  }
}
