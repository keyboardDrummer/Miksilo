package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Keyword
import core.deltas.Compilation
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}
import core.deltas.path.NodePath
import core.language.Language
import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.{PrimitiveType, Type}

object IntTypeDelta extends TypeInstance with StackType {

  override val key = IntTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = Seq.empty //TODO extend. long ?

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    new Keyword("I", false) ~> value(intType)
  }

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    "int" ~> value(intType)
  }

  val intType = new Node(IntTypeKey)

  override def getStackSize: Int = 1

  object IntTypeKey extends NodeShape

  override def description: String = "Defines the integer type."

  val constraintType = PrimitiveType("Int")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodePath, parentScope: Scope): Type = constraintType
}
