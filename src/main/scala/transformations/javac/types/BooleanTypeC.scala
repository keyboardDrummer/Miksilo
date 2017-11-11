package transformations.javac.types

import core.bigrammar.{BiGrammar, Keyword}
import core.particles.Language
import core.particles.grammars.LanguageGrammars
import core.particles.node.{Node, NodeClass}
import transformations.bytecode.types.{IntTypeC, StackType, TypeInstance}

object BooleanTypeC extends TypeInstance
  with StackType //TODO remove this and change VariablePool accordingly.
{
  override val key = BooleanTypeKey

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = Seq.empty

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    new Keyword("Z",false) ~> value(booleanType)
  }

  override def getStackType(_type: Node, state: Language): Node = IntTypeC.intType

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    "boolean" ~> value(booleanType)
  }

  def booleanType = new Node(BooleanTypeKey)


  object BooleanTypeKey extends NodeClass

  override def description: String = "Defines the boolean type."

  override def getStackSize: Int = IntTypeC.getStackSize
}
