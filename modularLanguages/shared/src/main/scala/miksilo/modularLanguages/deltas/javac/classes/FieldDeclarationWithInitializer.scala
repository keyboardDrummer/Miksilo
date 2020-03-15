package miksilo.modularLanguages.deltas.javac.classes

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.{NodePath, PathRoot}
import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.types.{TypeSkeleton, VoidTypeDelta}
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, VariableDelta}
import miksilo.modularLanguages.deltas.javac.statements.ExpressionAsStatementDelta
import miksilo.modularLanguages.deltas.statement.LocalDeclarationWithInitializerDelta.LocalDeclarationWithInitializer
import miksilo.modularLanguages.deltas.statement.assignment.SimpleAssignmentDelta
import miksilo.modularLanguages.deltas.statement.{BlockDelta, LocalDeclarationWithInitializerDelta}
import miksilo.modularLanguages.deltas.HasNameDelta.Name
import miksilo.modularLanguages.deltas.classes.ClassDelta.JavaClass
import miksilo.modularLanguages.deltas.classes.constructor.{ConstructorDelta, SuperCallExpression}
import miksilo.modularLanguages.deltas.javac.classes.FieldDeclarationDelta.{Field, Type}
import miksilo.modularLanguages.deltas.javac.methods.AccessibilityFieldsDelta
import miksilo.modularLanguages.deltas.method.MethodDelta
import miksilo.modularLanguages.deltas.method.call.CallDelta

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

object FieldDeclarationWithInitializer extends DeltaWithGrammar with DeltaWithPhase {

  override def description: String = "Enables fields to have initializers."

  override def dependencies: Set[Contract] = Set(FieldDeclarationDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val typeGrammar = find(TypeSkeleton.JavaTypeGrammar)
    val expression = find(ExpressionDelta.FirstPrecedenceGrammar)
    val fieldDeclarationGrammar = find(AccessibilityFieldsDelta.VisibilityField) ~ find(AccessibilityFieldsDelta.Static) ~
      typeGrammar.as(Type) ~~ identifier.as(Name) ~~ ("=" ~~> expression.as(LocalDeclarationWithInitializerDelta.Initializer)) ~< ";"  asNode Shape
    find(FieldDeclarationDelta.Shape).addAlternative(fieldDeclarationGrammar)
  }

  object Shape extends NodeShape

  def transformDeclarationWithInitializer(node: NodePath, initializerStatements: ArrayBuffer[Node], state: Language): Unit = {
    val localDeclaration: LocalDeclarationWithInitializer[NodePath] = node
    val field: Field[NodePath] = node
    val name: String = field.name

    val assignment = SimpleAssignmentDelta.neww(VariableDelta.neww(name), localDeclaration.initializer)
    val assignmentStatement = ExpressionAsStatementDelta.create(assignment)
    initializerStatements += assignmentStatement
    field.node.shape = FieldDeclarationDelta.Shape
    field.node.removeField(LocalDeclarationWithInitializerDelta.Initializer)
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val initializerStatements = new ArrayBuffer[Node]()
    val clazz: JavaClass[Node] = program
    PathRoot(program).visitShape(Shape, obj => transformDeclarationWithInitializer(obj, initializerStatements, compilation))

    if (initializerStatements.isEmpty)
      return

    val reversedInitializerStatements = initializerStatements.view.reverse //TODO: hack to fix the reverse hack in NodeLike.
    val fieldInitializerMethod = MethodDelta.neww(getFieldInitializerMethodName,VoidTypeDelta.voidType, Seq.empty,
      BlockDelta.neww(reversedInitializerStatements.toSeq))
    clazz.members = Seq(fieldInitializerMethod) ++ program.members

    for(constructor <- ConstructorDelta.getConstructors(program)) {
      val body = constructor.body.statements
      if (statementIsSuperCall(body.head)) {
        val bodyAfterHead = body.drop(1)
        val head = body.head
        val callToFieldInitialiser = ExpressionAsStatementDelta.create(CallDelta.neww(VariableDelta.neww(getFieldInitializerMethodName)))
        constructor.body.statements = Seq(head, callToFieldInitialiser) ++ bodyAfterHead
      }
    }
  }

  def getFieldInitializerMethodName: String = { //TODO make sure this name doesn't collide with other method names.
    "initialiseFields"
  }

  def statementIsSuperCall(statement: Node): Boolean = {
    statement.shape == ExpressionAsStatementDelta.shape &&
      ExpressionAsStatementDelta.getExpression(statement).shape == SuperCallExpression.Shape
  }
}
