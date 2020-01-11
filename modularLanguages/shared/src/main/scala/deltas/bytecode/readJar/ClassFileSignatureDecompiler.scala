package deltas.bytecode.readJar

import java.io.InputStream

import core.deltas.Delta
import deltas.bytecode.attributes.{ByteCodeAttribute, SignatureAttribute, UnParsedAttribute}
import deltas.javac.ByteCodeLanguage

object ClassFileSignatureDecompiler {

  val byteCodeParticles: Seq[Delta] = Seq(UnParsedAttribute) ++ ByteCodeLanguage.byteCodeWithoutTextualParser
  val onlySignatureAttribute: Seq[Delta] = byteCodeParticles.
    filter(particle => !particle.isInstanceOf[ByteCodeAttribute] || particle == SignatureAttribute)
  def getDecompiler(uri: String, fileStream: InputStream): Seq[Delta] = {
    Seq(new DecodeFileStream(uri, fileStream), ParseKnownAttributes, DecompileByteCodeSignature) ++ onlySignatureAttribute
  }
}
