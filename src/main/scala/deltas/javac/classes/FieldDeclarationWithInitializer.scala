package deltas.javac.classes

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}
import core.deltas.path.{NodePath, NodePathRoot}
import core.language.Language
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

  override def dependencies: Set[Contract] = Set(FieldDeclarationDelta)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val memberGrammar = find(ClassMemberGrammar)
    val fieldDeclarationWithInitializer = find(LocalDeclarationWithInitializerDelta.Shape).inner.asInstanceOf[NodeGrammar].inner asNode FieldWithInitializerKey
    memberGrammar.addOption(fieldDeclarationWithInitializer)
  }

  object FieldWithInitializerKey extends NodeShape
  override def description: String = "Enables fields to have initialisers."

  def transformDeclarationWithInitializer(node: NodePath, initializerStatements: ArrayBuffer[Node], state: Language): Unit = {
    val fieldWithInitialiser: LocalDeclarationWithInitializer[NodePath] = node
    val name: String = fieldWithInitialiser.name
    val _type = fieldWithInitialiser._type
    val declaration = FieldDeclarationDelta.field(_type, name)

    val assignment = AssignmentSkeleton.assignment(VariableDelta.variable(name), fieldWithInitialiser.initializer)
    val assignmentStatement = ExpressionAsStatementDelta.create(assignment)
    initializerStatements += assignmentStatement
    fieldWithInitialiser.node.replaceWith(declaration)
  }

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val initializerStatements = new ArrayBuffer[Node]()
    NodePathRoot(program).visit(obj => obj.shape match {
      case FieldWithInitializerKey => transformDeclarationWithInitializer(obj, initializerStatements, state)
      case _ =>
    })

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
    statement.shape == ExpressionAsStatementDelta.key &&
      ExpressionAsStatementDelta.getExpression(statement).shape == SuperCallExpression.SuperCall
  }
}
