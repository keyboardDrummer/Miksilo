package deltas.javac.types

import core.bigrammar.BiGrammar
import core.deltas.{Compilation, DeltaWithGrammar, HasShape}
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types._

object TypeApplicationDelta extends DeltaWithGrammar with HasType with HasShape {

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
    typeArgumentGrammar.addOption(typeGrammar)
    val typeApplication: BiGrammar = (objectInner.inner.as(Func) ~ ("<" ~> typeArgumentGrammar.someSeparated(",").as(Arguments) ~< ">")).
      asNode(Shape)
    typeGrammar.addOption(typeApplication)
  }

  object ByteCodeTypeArgumentGrammar extends GrammarKey
  def transformByteCodeGrammars(grammars: LanguageGrammars): Unit = {
    import grammars._
    val typeArgumentGrammar = create(ByteCodeTypeArgumentGrammar)
    val objectInner = find(QualifiedObjectTypeDelta.ByteCodeGrammarInner)
    objectInner.addOption((objectInner.inner.as(Func) ~ ("<" ~> typeArgumentGrammar.some.as(Arguments) ~< ">")).
      asNode(Shape))

    val typeGrammar = find(TypeSkeleton.ByteCodeTypeGrammar)
    val argumentGrammar = find(TypeApplicationDelta.ByteCodeTypeArgumentGrammar)
    argumentGrammar.addOption(typeGrammar)
  }

  override def getType(compilation: Compilation, builder: ConstraintBuilder, _type: NodeLike, parentScope: Scope): Type = {
    val funcType = TypeSkeleton.getType(compilation, builder, _type.func, parentScope)
    val argumentTypes = _type.arguments.map(t => TypeSkeleton.getType(compilation, builder, t, parentScope))
    core.smarts.types.objects.TypeApplication(funcType, argumentTypes, _type)
  }

  override def shape: NodeShape = Shape
}

