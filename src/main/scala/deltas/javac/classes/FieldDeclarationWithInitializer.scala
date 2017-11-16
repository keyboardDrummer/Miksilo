package deltas.javac.classes

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeClass}
import core.deltas.path.{Path, PathRoot}
import deltas.bytecode.types.VoidTypeC
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.constructor.{ConstructorC, SuperCallExpression}
import deltas.javac.methods.assignment.AssignmentSkeleton
import deltas.javac.methods.call.CallC
import deltas.javac.methods.{MethodDelta, VariableC}
import deltas.javac.statements.ExpressionAsStatementC
import deltas.javac.statements.locals.{LocalDeclarationC, LocalDeclarationWithInitializerC}

import scala.collection.mutable.ArrayBuffer
object FieldDeclarationWithInitializer extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(FieldDeclaration) ++ super.dependencies

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val memberGrammar = find(ClassMemberGrammar)
    val fieldDeclarationWithInitializer = find(LocalDeclarationWithInitializerC.Clazz).inner.asInstanceOf[NodeGrammar].inner asNode FieldWithInitializerKey
    memberGrammar.addOption(fieldDeclarationWithInitializer)
  }

  object FieldWithInitializerKey extends NodeClass
  override def description: String = "Enables fields to have initialisers."

  def transformDeclarationWithInitializer(fieldWithInitialiser: Path, initializerStatements: ArrayBuffer[Node], state: Language): Unit = {
    val name: String = LocalDeclarationC.getDeclarationName(fieldWithInitialiser)
    val _type = LocalDeclarationC.getDeclarationType(fieldWithInitialiser)
    val declaration = FieldDeclaration.field(_type, name)

    val assignment = AssignmentSkeleton.assignment(VariableC.variable(name), LocalDeclarationWithInitializerC.getInitializer(fieldWithInitialiser))
    val assignmentStatement = ExpressionAsStatementC.create(assignment)
    initializerStatements += assignmentStatement
    fieldWithInitialiser.replaceWith(declaration)
  }

  override def transform(program: Node, state: Compilation): Unit = {
    val initializerStatements = new ArrayBuffer[Node]()
    new PathRoot(program).visit(obj => obj.clazz match {
      case FieldWithInitializerKey => transformDeclarationWithInitializer(obj, initializerStatements, state)
      case _ =>
    })

    if (initializerStatements.isEmpty)
      return

    val reversedInitialiserStatements: ArrayBuffer[Node] = initializerStatements.reverse //TODO: hack to fix the reverse hack in NodeLike.

    val fieldInitializerMethod = MethodDelta.method(getFieldInitialiserMethodName,VoidTypeC.voidType, Seq.empty, reversedInitialiserStatements)
    program.members = Seq(fieldInitializerMethod) ++ program.members

    for(constructor <- program.members.filter(member => member.clazz == ConstructorC.ConstructorKey)) {
      val body = MethodDelta.getMethodBody(constructor)
      if (statementIsSuperCall(body.head)) {
        val bodyAfterHead = body.drop(1)
        val head = body.head
        val callToFieldInitialiser = ExpressionAsStatementC.create(CallC.call(VariableC.variable(getFieldInitialiserMethodName)))
        constructor(MethodDelta.Body) = Seq(head, callToFieldInitialiser) ++ bodyAfterHead
      }
    }
  }

  def getFieldInitialiserMethodName: String = { //TODO make sure this name doesn't collide with other method names.
    "initialiseFields"
  }

  def statementIsSuperCall(statement: Node): Boolean = {
    statement.clazz == ExpressionAsStatementC.key &&
      ExpressionAsStatementC.getExpression(statement).clazz == SuperCallExpression.SuperCall
  }
}
