package miksilo.modularLanguages.deltas.bytecode.attributes

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode._
import miksilo.modularLanguages.deltas.bytecode.readJar.ClassFileParser
import miksilo.modularLanguages.deltas.bytecode.readJar.ClassFileParser._

object SourceFileAttribute extends ByteCodeAttribute {

  object SourceFileAttributeKey extends NodeShape

  object SourceFileFileNameIndex extends NodeField

  def sourceFile(nameIndex: Int, fileNameIndex: Int): Node = new Node(SourceFileAttributeKey,
    SourceFileFileNameIndex -> fileNameIndex,
    AttributeNameKey -> nameIndex)

  def getBytes(compilation: Compilation, sourceFile: Node) = {
    shortToBytes(getSourceFileFileNameIndex(sourceFile))
  }

  def getSourceFileFileNameIndex(sourceFile: Node) = sourceFile(SourceFileFileNameIndex).asInstanceOf[Int]

  override def dependencies: Set[Contract] = Set(ByteCodeSkeleton)

  override def description: String = "Defines the source file attribute. It identifies which source file a particular class file was compiled from."

  override def shape = SourceFileAttributeKey

  override def getGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    ("NotImplemented" : BiGrammar).asNode(shape)
  } // TODO implement.

  override def constantPoolKey: String = "SourceFile"

  override def getParser(unParsed: Node): ClassFileParser.Parser[Node] = ParseShort.
    map(index => new Node(SourceFileAttributeKey, SourceFileFileNameIndex -> index))
}
