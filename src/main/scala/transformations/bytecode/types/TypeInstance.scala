package transformations.bytecode.types

import core.bigrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract, ParticleWithGrammar}
import transformations.bytecode.constants.ConstantEntry
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}

trait TypeInstance extends ParticleWithGrammar with ConstantEntry {
  val key: Key

  override def inject(state: CompilationState): Unit = {
    TypeSkeleton.getSuperTypesRegistry(state).put(key, _type => getSuperTypes(_type, state))
    TypeSkeleton.getState(state).instances.put(key, this)
    super.inject(state)
  }

  def getSuperTypes(_type: Node, state: CompilationState): Seq[Node]

  def getStackType(_type: Node, state: CompilationState) = _type

  def getByteCodeGrammar(grammars: GrammarCatalogue): BiGrammar

  override def dependencies: Set[Contract] = Set(TypeSkeleton, ByteCodeSkeleton)

  override def getByteCode(constant: Node, state: CompilationState): Seq[Byte] = {
    PrintByteCode.toUTF8ConstantEntry(TypeSkeleton.getByteCodeString(state)(constant))
  }

  def byteCodeGrammarKey = (key,"bytecode")
  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val javaGrammar: BiGrammar = getJavaGrammar(grammars)
    grammars.create(key, javaGrammar)
    val parseType = grammars.find(TypeSkeleton.JavaTypeGrammar)
    parseType.addOption(javaGrammar)

    val byteCodeGrammar = grammars.create(byteCodeGrammarKey, getByteCodeGrammar(grammars))
    val byteCodeType = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    byteCodeType.addOption(byteCodeGrammar)
    super.transformGrammars(grammars)
  }

  def getJavaGrammar(grammars: GrammarCatalogue): BiGrammar

  override def getConstantEntryGrammar(grammars: GrammarCatalogue): BiGrammar = {
    "T" ~> grammars.find(byteCodeGrammarKey)
  }
}
