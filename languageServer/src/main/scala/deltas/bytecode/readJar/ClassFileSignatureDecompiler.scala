package deltas.bytecode.readJar

import core.deltas.Delta
import deltas.bytecode.attributes.{ByteCodeAttribute, SignatureAttribute, UnParsedAttribute}
import deltas.javac.JavaLanguage

object ClassFileSignatureDecompiler {

  val byteCodeParticles: Seq[Delta] = Seq(DecodeByteCodeParser, UnParsedAttribute) ++ JavaLanguage.byteCodeWithoutTextualParser
  val onlySignatureAttribute: Seq[Delta] = byteCodeParticles.
    filter(particle => !particle.isInstanceOf[ByteCodeAttribute] || particle == SignatureAttribute)
  def getDecompiler: Seq[Delta] = {
    Seq(ParseKnownAttributes, DecompileByteCodeSignature) ++ onlySignatureAttribute
  }
}
