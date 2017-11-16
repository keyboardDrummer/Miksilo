package deltas.bytecode.readJar

import java.io.{BufferedInputStream, InputStream}

import core.grammar.ParseException
import core.deltas.node.Node
import core.deltas.{Language, Delta}
import deltas.bytecode.attributes.{ByteCodeAttribute, SignatureAttribute, UnParsedAttribute}
import deltas.javac.JavaCompilerDeltas



object ClassFileSignatureDecompiler {

  val byteCodeParticles: Seq[Delta] = Seq(UnParsedAttribute) ++ JavaCompilerDeltas.byteCodeWithoutTextualParser ++ Seq(DecodeByteCodeParser)
  val onlySignatureAttribute: Seq[Delta] = byteCodeParticles.
    filter(particle => !particle.isInstanceOf[ByteCodeAttribute] || particle == SignatureAttribute)
  def getDecompiler: Seq[Delta] = {
    Seq(ParseKnownAttributes, DecompileByteCodeSignature) ++ onlySignatureAttribute
  }
}
