package deltas.bytecode.readJar

import core.deltas.Delta
import deltas.bytecode.attributes.{ByteCodeAttribute, SignatureAttribute, UnParsedAttribute}
import deltas.javac.ByteCodeLanguage

object ClassFileSignatureDecompiler {

  val byteCodeParticles: Seq[Delta] = Seq(UnParsedAttribute) ++ ByteCodeLanguage.byteCodeWithoutTextualParser
  val onlySignatureAttribute: Seq[Delta] = byteCodeParticles.
    filter(particle => !particle.isInstanceOf[ByteCodeAttribute] || particle == SignatureAttribute)
  def getDecompiler: Seq[Delta] = {
    Seq(DecodeByteCodeParser, ParseKnownAttributes, DecompileByteCodeSignature) ++ onlySignatureAttribute
  }
}
