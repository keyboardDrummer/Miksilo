package miksilo.modularLanguages.deltas.javac.types

import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{Node, NodeLike, NodeShape}
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar, HasShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{Type, TypeVariable}
import miksilo.modularLanguages.deltas.bytecode.types.HasTypeDelta

object WildcardTypeArgument extends DeltaWithGrammar with HasTypeDelta with HasShape {

  override def description: String = "Adds the wildcard type argument '*'."

  object Shape extends NodeShape
  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val byteCodeArgumentGrammar = find(TypeApplicationDelta.ByteCodeTypeArgumentGrammar)
    byteCodeArgumentGrammar.addAlternative("*" ~> value(new Node(Shape)))

    val javaArgumentGrammar = find(TypeApplicationDelta.JavaTypeArgumentGrammar)
    javaArgumentGrammar.addAlternative("?" ~> value(new Node(Shape)))
  }

  override def getType(compilation: Compilation, builder: ConstraintBuilder, path: NodeLike, parentScope: Scope): Type = {
    TypeVariable("?") //TODO not sure what to do here.
  }

  override def shape: NodeShape = Shape

  override def dependencies: Set[Contract] = Set(TypeApplicationDelta)
}

