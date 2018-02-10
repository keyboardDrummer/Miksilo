package deltas.bytecode.types

import core.bigrammar.BiGrammar
import core.bigrammar.grammars.BiFailure
import core.deltas.Compilation
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.{ConcreteScope, Scope}
import core.smarts.types.objects.{Type, TypeFromDeclaration}

object UnqualifiedObjectTypeDelta extends TypeInstance {
  override val shape: Shape.type = Shape

  override def getSuperTypes(_type: Node, state: Language): Seq[Node] = {
    Seq.empty //TODO extend
  }

  object AnyObjectTypeGrammar extends GrammarKey
  override def getJavaGrammar(grammars: LanguageGrammars): BiGrammar = {
    import grammars._
    val result = identifier.as(Name).asLabelledNode(Shape)
    create(AnyObjectTypeGrammar, result | find(QualifiedObjectTypeDelta.Shape))
    result
  }

  def neww(className: String): Node = Shape.create(Name -> className)

  def getName(objectType: NodeLike): String = objectType(Name).asInstanceOf[String]

  object Name extends NodeField

  object Shape extends NodeShape

  override def description: String = "Defines the object type based on a single identifier."

  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = {
    val name = getName(_type)
    if (name == "String" && parentScope.isInstanceOf[ConcreteScope] && parentScope.asInstanceOf[ConcreteScope].debugName != "classInternalScope")
      System.out.append("")
    TypeFromDeclaration(builder.resolve2(name, _type.asPath, parentScope))
  }

  override def getByteCodeGrammar(grammars: LanguageGrammars): BiGrammar = BiFailure("TODO make changes do this implementation can be removed.") //TODO make changes do this implementation can be removed.
}
