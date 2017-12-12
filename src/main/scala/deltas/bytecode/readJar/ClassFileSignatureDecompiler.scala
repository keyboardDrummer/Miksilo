package deltas.bytecode.readJar

import core.deltas.Delta
import deltas.bytecode.attributes.{ByteCodeAttribute, SignatureAttribute, UnParsedAttribute}
import deltas.javac.JavaCompilerDeltas

object ClassFileSignatureDecompiler {

  val byteCodeParticles: Seq[Delta] = Seq(DecodeByteCodeParser, UnParsedAttribute) ++ JavaCompilerDeltas.byteCodeWithoutTextualParser
  val onlySignatureAttribute: Seq[Delta] = byteCodeParticles.
    filter(particle => !particle.isInstanceOf[ByteCodeAttribute] || particle == SignatureAttribute)
  def getDecompiler: Seq[Delta] = {
    Seq(ParseKnownAttributes, DecompileByteCodeSignature) ++ onlySignatureAttribute
  }
}
