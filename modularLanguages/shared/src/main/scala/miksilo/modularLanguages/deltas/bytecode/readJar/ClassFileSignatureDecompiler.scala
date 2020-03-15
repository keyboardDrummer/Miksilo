package miksilo.modularLanguages.deltas.bytecode.readJar

import java.io.InputStream

import miksilo.modularLanguages.core.deltas.Delta
import miksilo.modularLanguages.deltas.bytecode.ByteCodeLanguage
import miksilo.modularLanguages.deltas.bytecode.attributes.{ByteCodeAttribute, SignatureAttribute, UnParsedAttribute}

object ClassFileSignatureDecompiler {

  val byteCodeParticles: Seq[Delta] = Seq(UnParsedAttribute) ++ ByteCodeLanguage.byteCodeWithoutTextualParser
  val onlySignatureAttribute: Seq[Delta] = byteCodeParticles.
    filter(particle => !particle.isInstanceOf[ByteCodeAttribute] || particle == SignatureAttribute)
  def getDecompiler(uri: String, fileStream: InputStream): Seq[Delta] = {
    Seq(new DecodeFileStream(uri, fileStream), ParseKnownAttributes, DecompileByteCodeSignature) ++ onlySignatureAttribute
  }
}
