package miksilo.modularLanguages.deltas.smithy

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.modularLanguages.core.node.{NodeField, NodeLike, NodeShape, NodeWrapper}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.PrimitiveType
import miksilo.modularLanguages.deltas.{ConstraintSkeleton, HasNameDelta}
import miksilo.modularLanguages.core.deltas.path.ConstraintBuilderExtension._
import miksilo.modularLanguages.deltas.classes.HasConstraintsDelta

object OperationDelta extends DeltaWithGrammar with HasConstraintsDelta {

  object Shape extends NodeShape
  object ArgumentType extends NodeField
  object ReturnType extends NodeField
  object ErrorTypes extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val name = find(HasNameDelta.Name)
    val shapeIdentifier = find(RelativeShapeIdentifierDelta.ShapeIdentifierGrammar)
    val operationErrors = "errors" ~~> "[" ~> shapeIdentifier.manySeparated(",") ~< "]"
    val operationResults = ("->" ~> shapeIdentifier).option.optionToSeq.as(ReturnType) ~~ operationErrors.option.flattenOptionSeq.as(ErrorTypes)
    val grammar = "operation" ~~ name ~~ shapeIdentifier.option.optionToSeq.as(ArgumentType).inParenthesis ~~ operationResults asNode Shape
    val members = find(ShapeStatementDelta.ShapeBody)
    members.addAlternative(grammar)
  }

  override def description = "Adds the namespace service statement"

  override def dependencies = Set(ShapeStatementDelta)

  override def shape = Shape

  val operationType = PrimitiveType("operation")

  implicit class Operation[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def argumentType: Seq[T] = node(ArgumentType).asInstanceOf[Seq[T]]
    def returnType: Seq[T] = node(ReturnType).asInstanceOf[Seq[T]]
    def errorTypes: Seq[T] = node(ErrorTypes).asInstanceOf[Seq[T]]
  }

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    builder.declare(path.getField(HasNameDelta.Name), parentScope, operationType)
    val operation: Operation[NodePath] = path
    operation.argumentType.foreach(argumentType => {
      ConstraintSkeleton.constraints(compilation, builder, argumentType, parentScope)
    })
    operation.returnType.foreach(returnType => {
      ConstraintSkeleton.constraints(compilation, builder, returnType, parentScope)
    })
    operation.errorTypes.foreach(errorType => {
      ConstraintSkeleton.constraints(compilation, builder, errorType, parentScope)
    })
  }
}
