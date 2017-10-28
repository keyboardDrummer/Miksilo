package transformations.bytecode.types

import core.bigrammar.BiGrammar
import core.particles.grammars.{GrammarCatalogue, KeyGrammar}
import core.particles.node.{Node, NodeClass}
import core.particles.{Contract, DeltaWithGrammar, Language}
import transformations.bytecode.ByteCodeSkeleton

trait TypeInstance extends DeltaWithGrammar {
  val key: NodeClass

  override def inject(state: Language): Unit = {
    TypeSkeleton.getSuperTypesRegistry(state).put(key, _type => getSuperTypes(_type, state))
    TypeSkeleton.getRegistry(state).instances.put(key, this)
    super.inject(state)
  }

  def getSuperTypes(_type: Node, state: Language): Seq[Node]

  def getStackType(_type: Node, state: Language): Node = _type

  def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar

  override def dependencies: Set[Contract] = Set(TypeSkeleton, ByteCodeSkeleton)

  def byteCodeGrammarKey = KeyGrammar(key)
  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val javaGrammar: BiGrammar = getJavaGrammar(grammars)
    grammars.create(key, javaGrammar)
    val parseType = grammars.find(TypeSkeleton.JavaTypeGrammar)
    parseType.addOption(javaGrammar)

    val byteCodeGrammar = grammars.create(byteCodeGrammarKey, getByteCodeGrammar(grammars))
    val byteCodeType = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    byteCodeType.addOption(byteCodeGrammar)
  }

  def getJavaGrammar(grammars: GrammarCatalogue): BiGrammar
}
