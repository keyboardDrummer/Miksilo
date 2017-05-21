package transformations.bytecode.readJar

import java.io.{BufferedInputStream, InputStream}

import core.grammar.ParseException
import core.particles.node.Node
import core.particles.{CompilationState, Delta}
import transformations.bytecode.attributes.{ByteCodeAttribute, SignatureAttribute, UnParsedAttribute}
import transformations.javac.JavaCompiler



object ClassFileSignatureDecompiler {

  val byteCodeParticles: Seq[Delta] = Seq(UnParsedAttribute) ++ JavaCompiler.byteCodeWithoutTextualParser ++ Seq(DecodeByteCodeParser)
  val onlySignatureAttribute: Seq[Delta] = byteCodeParticles.
    filter(particle => !particle.isInstanceOf[ByteCodeAttribute] || particle == SignatureAttribute)
  def getDecompiler: Seq[Delta] = {
    Seq(ParseKnownAttributes, DecompileByteCodeSignature) ++ onlySignatureAttribute
  }
}
