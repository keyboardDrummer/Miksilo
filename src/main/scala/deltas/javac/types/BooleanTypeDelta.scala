package deltas.javac.types

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
import deltas.bytecode.types.{IntTypeDelta, StackType, TypeInstance}

object BooleanTypeDelta extends TypeInstance
  with StackType //TODO remove this and change VariablePool accordingly.
{
  val constraintType: Type = PrimitiveType("Boolean")

  override val key = BooleanTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = Seq.empty

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    new Keyword("Z",false) ~> value(booleanType)
  }

  override def getStackType(_type: Node, state: Language): Node = IntTypeDelta.intType

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    "boolean" ~> value(booleanType)
  }

  def booleanType = new Node(BooleanTypeKey)


  object BooleanTypeKey extends NodeShape

  override def description: String = "Defines the boolean type."

  override def getStackSize: Int = IntTypeDelta.getStackSize

  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodePath, parentScope: Scope): Type = constraintType
}
