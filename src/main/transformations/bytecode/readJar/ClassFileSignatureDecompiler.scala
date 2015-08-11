package transformations.bytecode.readJar

import java.io.{BufferedInputStream, InputStream}

import core.grammar.ParseException
import core.particles.node.Node
import core.particles.{CompilationState, Particle}
import transformations.bytecode.attributes.{ByteCodeAttribute, SignatureAttribute, UnParsedAttribute}
import transformations.javac.JavaCompiler



object ClassFileSignatureDecompiler {

  val byteCodeParticles: Seq[Particle] = Seq(UnParsedAttribute) ++ JavaCompiler.byteCodeWithoutTextualParser ++ Seq(DecodeByteCodeParser)
  val onlySignatureAttribute: Seq[Particle] = byteCodeParticles.
    filter(particle => !particle.isInstanceOf[ByteCodeAttribute] || particle == SignatureAttribute)
  def getDecompiler = {
    Seq(ParseKnownAttributes, DecompileByteCodeSignature) ++ onlySignatureAttribute
  }
}
