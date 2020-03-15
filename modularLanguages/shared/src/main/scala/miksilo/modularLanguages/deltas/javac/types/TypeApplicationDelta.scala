package miksilo.modularLanguages.deltas.javac.types

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar, HasShape}
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.modularLanguages.deltas.bytecode.types._

object TypeApplicationDelta extends DeltaWithGrammar with HasTypeDelta with HasShape {

  override def description: String = "Adds application of generic types"

  object Shape extends NodeShape
  object Func extends NodeField
  object Arguments extends NodeField

  implicit class TypeApplication[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def func: T = node(Func).asInstanceOf[T]
    def arguments: Seq[T] = node(Arguments).asInstanceOf[Seq[T]]
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    transformByteCodeGrammars(grammars)
    transformJavaGrammars(grammars)
  }

  object JavaTypeArgumentGrammar extends GrammarKey
  def transformJavaGrammars(grammars: LanguageGrammars): Unit = {
    import grammars._
    val typeArgumentGrammar = create(JavaTypeArgumentGrammar)
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val objectInner = find(UnqualifiedObjectTypeDelta.AnyObjectTypeGrammar)
    typeArgumentGrammar.addAlternative(typeGrammar)
    val typeApplication: BiGrammar = (objectInner.inner.as(Func) ~ ("<" ~> typeArgumentGrammar.someSeparated(",").as(Arguments) ~< ">")).
      asNode(Shape)
    typeGrammar.addAlternative(typeApplication)
  }

  object ByteCodeTypeArgumentGrammar extends GrammarKey
  def transformByteCodeGrammars(grammars: LanguageGrammars): Unit = {
    import grammars._
    val typeArgumentGrammar = create(ByteCodeTypeArgumentGrammar)
    val objectInner = find(QualifiedObjectTypeDelta.ByteCodeGrammarInner)
    objectInner.addAlternative((objectInner.inner.as(Func) ~ ("<" ~> typeArgumentGrammar.some.as(Arguments) ~< ">")).
      asNode(Shape))

    val typeGrammar = find(TypeSkeleton.ByteCodeTypeGrammar)
    val argumentGrammar = find(ByteCodeTypeArgumentGrammar)
    argumentGrammar.addAlternative(typeGrammar)
  }

  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = {
    val funcType = TypeSkeleton.getType(compilation, builder, _type.func, parentScope)
    val argumentTypes = _type.arguments.map(t => TypeSkeleton.getType(compilation, builder, t, parentScope))
    core.smarts.types.objects.TypeApplication(funcType, argumentTypes, _type)
  }

  override def shape: NodeShape = Shape

  override def dependencies: Set[Contract] = Set(QualifiedObjectTypeDelta, TypeSkeleton)
}

