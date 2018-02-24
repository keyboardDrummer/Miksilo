package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.Keyword
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeLike, NodeShape}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{PrimitiveType, Type}

object ShortTypeDelta extends ByteCodeTypeInstance with HasStackTypeDelta {

  override val shape = ShortTypeKey

  override def getSuperTypes(_type: Node): Seq[Node] = Seq.empty //TODO extend. long ?

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    new Keyword("S",false) ~> value(shortType)
  }

  override def getJavaGrammar(grammars: LanguageGrammars) = {
    import grammars._
    "short" ~> value(shortType)
  }

  def shortType = new Node(ShortTypeKey)

  override def getStackSize: Int = 1

  object ShortTypeKey extends NodeShape

  override def description: String = "Defines the short type."

  val constraintType = PrimitiveType("Short")
  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = constraintType
}
