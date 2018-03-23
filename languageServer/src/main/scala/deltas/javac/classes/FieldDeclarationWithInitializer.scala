package deltas.javac.classes

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node.{Node, NodeGrammar, NodeShape}
import core.deltas.path.{NodePath, PathRoot}
import core.language.{Compilation, Language}
import deltas.bytecode.types.VoidTypeDelta
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.constructor.{ConstructorDelta, SuperCallExpression}
import deltas.javac.methods.assignment.AssignmentSkeleton
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{MethodDelta, VariableDelta}
import deltas.javac.statements.ExpressionAsStatementDelta
import deltas.javac.statements.locals.LocalDeclarationWithInitializerDelta
import deltas.javac.statements.locals.LocalDeclarationWithInitializerDelta.LocalDeclarationWithInitializer

import scala.collection.mutable.ArrayBuffer
object FieldDeclarationWithInitializer extends DeltaWithGrammar with DeltaWithPhase {

  override def description: String = "Enables fields to have initialisers."

  override def dependencies: Set[Contract] = Set(FieldDeclarationDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val memberGrammar = find(ClassMemberGrammar)
    val fieldDeclarationWithInitializer = find(LocalDeclarationWithInitializerDelta.Shape).inner.asInstanceOf[NodeGrammar].inner asNode Shape
    memberGrammar.addAlternative(fieldDeclarationWithInitializer)
  }

  object Shape extends NodeShape

  def transformDeclarationWithInitializer(node: NodePath, initializerStatements: ArrayBuffer[Node], state: Language): Unit = {
    val field: LocalDeclarationWithInitializer[NodePath] = node
    val name: String = field.name
    val fieldDeclaration = FieldDeclarationDelta.field(field._type, name)

    val assignment = AssignmentSkeleton.assignment(VariableDelta.variable(name), field.initializer)
    val assignmentStatement = ExpressionAsStatementDelta.create(assignment)
    initializerStatements += assignmentStatement
    field.node.replaceWith(fieldDeclaration)
  }

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val initializerStatements = new ArrayBuffer[Node]()
    PathRoot(program).visitShape(Shape, obj => transformDeclarationWithInitializer(obj, initializerStatements, compilation))

    if (initializerStatements.isEmpty)
      return

    val reversedInitialiserStatements: ArrayBuffer[Node] = initializerStatements.reverse //TODO: hack to fix the reverse hack in NodeLike.

    val fieldInitializerMethod = MethodDelta.method(getFieldInitialiserMethodName,VoidTypeDelta.voidType, Seq.empty, reversedInitialiserStatements)
    program.members = Seq(fieldInitializerMethod) ++ program.members

    for(constructor <- ConstructorDelta.getConstructors(program)) {
      val body = constructor.body
      if (statementIsSuperCall(body.head)) {
        val bodyAfterHead = body.drop(1)
        val head = body.head
        val callToFieldInitialiser = ExpressionAsStatementDelta.create(CallDelta.call(VariableDelta.variable(getFieldInitialiserMethodName)))
        constructor(MethodDelta.Body) = Seq(head, callToFieldInitialiser) ++ bodyAfterHead
      }
    }
  }

  def getFieldInitialiserMethodName: String = { //TODO make sure this name doesn't collide with other method names.
    "initialiseFields"
  }

  def statementIsSuperCall(statement: Node): Boolean = {
    statement.shape == ExpressionAsStatementDelta.shape &&
      ExpressionAsStatementDelta.getExpression(statement).shape == SuperCallExpression.SuperCall
  }
}
