package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node}
import core.particles.path.{Path, PathRoot}
import core.particles._
import transformations.bytecode.types.VoidTypeC
import transformations.javac.classes.skeleton.JavaClassSkeleton._
import transformations.javac.constructor.{ConstructorC, SuperCallExpression}
import transformations.javac.methods.assignment.AssignmentSkeleton
import transformations.javac.methods.call.CallC
import transformations.javac.methods.{MethodC, VariableC}
import transformations.javac.statements.ExpressionAsStatementC
import transformations.javac.statements.locals.{LocalDeclarationC, LocalDeclarationWithInitializerC}

import scala.collection.mutable.ArrayBuffer
object FieldDeclarationWithInitializer extends DeltaWithGrammar with DeltaWithPhase {

  override def dependencies: Set[Contract] = Set(FieldDeclaration) ++ super.dependencies

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val memberGrammar = grammars.find(ClassMemberGrammar)
    val fieldDeclarationWithInitializer = grammars.find(LocalDeclarationWithInitializerC).asNode(FieldWithInitializerKey, FromMap)
    memberGrammar.addOption(fieldDeclarationWithInitializer)
  }

  object FieldWithInitializerKey extends Key
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

    val fieldInitializerMethod = MethodC.method(getFieldInitialiserMethodName,VoidTypeC.voidType, Seq.empty, reversedInitialiserStatements)
    program.members = Seq(fieldInitializerMethod) ++ program.members

    for(constructor <- program.members.filter(member => member.clazz == ConstructorC.ConstructorKey)) {
      val body = MethodC.getMethodBody(constructor)
      if (statementIsSuperCall(body.head)) {
        val bodyAfterHead = body.drop(1)
        val head = body.head
        val callToFieldInitialiser = ExpressionAsStatementC.create(CallC.call(VariableC.variable(getFieldInitialiserMethodName)))
        constructor(MethodC.MethodBodyKey) = Seq(head, callToFieldInitialiser) ++ bodyAfterHead
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
