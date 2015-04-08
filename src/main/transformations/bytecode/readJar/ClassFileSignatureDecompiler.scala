package transformations.bytecode.readJar

import java.io.{BufferedInputStream, InputStream}

import core.grammar.ParseException
import core.particles.node.Node
import core.particles.{CompilationState, Particle}
import transformations.bytecode.attributes.{ByteCodeAttribute, SignatureAttribute, UnParsedAttribute}
import transformations.javac.JavaCompiler

object DecodeByteCodeParser extends Particle {
  override def inject(state: CompilationState): Unit = {
    state.parse = decodeStream
  }

  def decodeStream(inputStream: InputStream): Node = {
    val bis = new BufferedInputStream(inputStream)
    val inputBytes = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte)
    val parseResult = ClassFileParser.classFileParser(new ArrayReader(0, inputBytes))
    if (!parseResult.successful)
      throw new ParseException(parseResult.toString)

    val clazz = parseResult.get
    clazz
  }

  override def description: String = "Decodes a binary bytecode classfile."
}

object ClassFileSignatureDecompiler {

  val byteCodeParticles: Seq[Particle] = Seq(UnParsedAttribute) ++ JavaCompiler.byteCodeWithoutTextualParser ++ Seq(DecodeByteCodeParser)
  val onlySignatureAttribute: Seq[Particle] = byteCodeParticles.
    filter(particle => !particle.isInstanceOf[ByteCodeAttribute] || particle == SignatureAttribute)
  def getDecompiler = {
    Seq(ParseKnownAttributes, DecompileByteCodeSignature) ++ onlySignatureAttribute
  }
}
