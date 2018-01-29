package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.deltas.grammars.{KeyGrammar, LanguageGrammars}
import core.deltas.node.{Node, NodeShape}
import core.deltas.path.NodePath
import core.deltas.{Compilation, Contract, DeltaWithGrammar}
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.Type
import deltas.bytecode.ByteCodeSkeleton

trait TypeInstance extends DeltaWithGrammar {
  val key: NodeShape

  override def inject(state: Language): Unit = {
    TypeSkeleton.getSuperTypesRegistry(state).put(key, _type => getSuperTypes(_type, state))
    TypeSkeleton.getRegistry(state).instances.put(key, this)
    super.inject(state)
  }

  def getSuperTypes(_type: Node, state: Language): Seq[Node]

  def getStackType(_type: Node, state: Language): Node = _type

  def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar

  override def dependencies: Set[Contract] = Set(TypeSkeleton, ByteCodeSkeleton)

  def byteCodeGrammarKey = KeyGrammar(key)
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    val javaGrammar: BiGrammar = getJavaGrammar(grammars)
    grammars.create(key, javaGrammar)
    val parseType = grammars.find(TypeSkeleton.JavaTypeGrammar)
    parseType.addOption(javaGrammar)

    val byteCodeGrammar = grammars.create(byteCodeGrammarKey, getByteCodeGrammar(grammars))
    val byteCodeType = grammars.find(TypeSkeleton.ByteCodeTypeGrammar)
    byteCodeType.addOption(byteCodeGrammar)
  }

  def getJavaGrammar(grammars: LanguageGrammars): BiGrammar

  def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodePath, parentScope: Scope): Type
}
