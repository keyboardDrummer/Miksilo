package deltas.javac.classes

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeShape}
import core.deltas.path.{NodePath, NodePathRoot}
import core.language.Language
import deltas.bytecode.types.VoidTypeC
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.constructor.{ConstructorDelta, SuperCallExpression}
import deltas.javac.methods.assignment.AssignmentSkeleton
import deltas.javac.methods.call.CallDelta
import deltas.javac.methods.{MethodDelta, VariableDelta}
import deltas.javac.statements.ExpressionAsStatementDelta
import deltas.javac.statements.locals.{LocalDeclarationDelta, LocalDeclarationWithInitializerDelta}

import scala.collection.mutable.ArrayBuffer
object FieldDeclarationWithInitializer extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(FieldDeclaration)

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val memberGrammar = find(ClassMemberGrammar)
    val fieldDeclarationWithInitializer = find(LocalDeclarationWithInitializerDelta.Shape).inner.asInstanceOf[NodeGrammar].inner asNode FieldWithInitializerKey
    memberGrammar.addOption(fieldDeclarationWithInitializer)
  }

  object FieldWithInitializerKey extends NodeShape
  override def description: String = "Enables fields to have initialisers."

  def transformDeclarationWithInitializer(fieldWithInitialiser: NodePath, initializerStatements: ArrayBuffer[Node], state: Language): Unit = {
    val name: String = LocalDeclarationDelta.getDeclarationName(fieldWithInitialiser)
    val _type = LocalDeclarationDelta.getDeclarationType(fieldWithInitialiser)
    val declaration = FieldDeclaration.field(_type, name)

    val assignment = AssignmentSkeleton.assignment(VariableDelta.variable(name), LocalDeclarationWithInitializerDelta.getInitializer(fieldWithInitialiser))
    val assignmentStatement = ExpressionAsStatementDelta.create(assignment)
    initializerStatements += assignmentStatement
    fieldWithInitialiser.replaceWith(declaration)
  }

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val initializerStatements = new ArrayBuffer[Node]()
    new NodePathRoot(program).visit(obj => obj.shape match {
      case FieldWithInitializerKey => transformDeclarationWithInitializer(obj, initializerStatements, state)
      case _ =>
    })

    if (initializerStatements.isEmpty)
      return

    val reversedInitialiserStatements: ArrayBuffer[Node] = initializerStatements.reverse //TODO: hack to fix the reverse hack in NodeLike.

    val fieldInitializerMethod = MethodDelta.method(getFieldInitialiserMethodName,VoidTypeC.voidType, Seq.empty, reversedInitialiserStatements)
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
