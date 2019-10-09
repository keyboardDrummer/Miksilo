package deltas.javac.classes

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.{NodePath, PathRoot}
import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import deltas.bytecode.types.{TypeSkeleton, VoidTypeDelta}
import deltas.expression.{ExpressionDelta, VariableDelta}
import deltas.javac.classes.FieldDeclarationDelta.{Field, Type}
import deltas.javac.classes.skeleton.JavaClassDelta._
import deltas.javac.constructor.{ConstructorDelta, SuperCallExpression}
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{AccessibilityFieldsDelta, MethodDelta}
import deltas.javac.statements.ExpressionAsStatementDelta
import deltas.statement.LocalDeclarationWithInitializerDelta.LocalDeclarationWithInitializer
import deltas.statement.assignment.SimpleAssignmentDelta
import deltas.statement.{BlockDelta, LocalDeclarationWithInitializerDelta}

import deltas.HasNameDelta.Name

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
    PathRoot(program).visitShape(Shape, obj => transformDeclarationWithInitializer(obj, initializerStatements, compilation))

    if (initializerStatements.isEmpty)
      return

    val reversedInitializerStatements: ArrayBuffer[Node] = initializerStatements.reverse //TODO: hack to fix the reverse hack in NodeLike.

    val fieldInitializerMethod = MethodDelta.neww(getFieldInitializerMethodName,VoidTypeDelta.voidType, Seq.empty,
      BlockDelta.neww(reversedInitializerStatements))
    program.members = Seq(fieldInitializerMethod) ++ program.members

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
