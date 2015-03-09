package transformations.types

import core.biGrammar.BiGrammar
import core.particles.grammars.GrammarCatalogue
import core.particles.{CompilationState, Contract, MetaObject, ParticleWithGrammar}
import transformations.bytecode.constants.ConstantEntry
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}

trait TypeInstance extends ParticleWithGrammar with ConstantEntry  {

  val key: AnyRef

  override def inject(state: CompilationState): Unit = {
    TypeSkeleton.getSuperTypesRegistry(state).put(key, _type => getSuperTypes(_type, state))
    TypeSkeleton.getState(state).toByteCodeString.put(key, _type => getByteCodeString(_type, state))
    TypeSkeleton.getState(state).stackSize.put(key, getStackSize)
    super.inject(state)
  }

  def getSuperTypes(_type: MetaObject, state: CompilationState): Seq[MetaObject]

  def getStackType(_type: MetaObject) = _type

  def getByteCodeString(_type: MetaObject, state: CompilationState): String

  def getStackSize: Int

  override def dependencies: Set[Contract] = Set(TypeSkeleton, ByteCodeSkeleton)

  override def getByteCode(constant: MetaObject, state: CompilationState): Seq[Byte] = {
    PrintByteCode.toUTF8ConstantEntry(getByteCodeString(constant, state))
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val javaGrammar: BiGrammar = getJavaGrammar(grammars)
    grammars.create(key, javaGrammar)
    val parseType = grammars.find(TypeSkeleton.TypeGrammar)
    parseType.addOption(javaGrammar)
    super.transformGrammars(grammars)
  }

  def getJavaGrammar(grammars: GrammarCatalogue): BiGrammar

  override def getGrammar(grammars: GrammarCatalogue): BiGrammar = {
    "T" ~> grammars.find(key)
  }
}
