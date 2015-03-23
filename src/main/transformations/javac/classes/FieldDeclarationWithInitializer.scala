package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.{Path, Root}
import core.particles.{CompilationState, Contract, ParticleWithGrammar, ParticleWithPhase}
import transformations.javac.constructor.{ConstructorC, SuperCallExpression}
import transformations.javac.methods.{CallC, MethodC, VariableC}
import transformations.javac.methods.assignment.AssignmentSkeleton
import transformations.javac.statements.ExpressionAsStatementC
import transformations.javac.statements.locals.{LocalDeclarationC, LocalDeclarationWithInitializerC}
import transformations.types.VoidTypeC

import scala.collection.mutable.ArrayBuffer

object FieldDeclarationWithInitializer extends ParticleWithGrammar with ParticleWithPhase {

  override def dependencies: Set[Contract] = Set(FieldDeclaration) ++ super.dependencies

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val memberGrammar = grammars.find(JavaClassSkeleton.ClassMemberGrammar)
    val fieldDeclarationWithInitializer = grammars.find(LocalDeclarationWithInitializerC) ^^ parseMap(FieldWithInitializerKey, PartialSelf)
    memberGrammar.addOption(fieldDeclarationWithInitializer)
  }

  object FieldWithInitializerKey
  override def description: String = "Enables fields to have initialisers."

  def transformDeclarationWithInitializer(fieldWithInitialiser: Path, initializerStatements: ArrayBuffer[Node], state: CompilationState): Unit = {
    val name: String = LocalDeclarationC.getDeclarationName(fieldWithInitialiser)
    val _type = LocalDeclarationC.getDeclarationType(fieldWithInitialiser)
    val declaration = FieldDeclaration.field(_type, name)

    val assignment = AssignmentSkeleton.assignment(VariableC.variable(name), LocalDeclarationWithInitializerC.getInitializer(fieldWithInitialiser))
    val assignmentStatement = ExpressionAsStatementC.asStatement(assignment)
    initializerStatements += assignmentStatement
    fieldWithInitialiser.replaceWith(declaration)

  }

  override def transform(program: Node, state: CompilationState): Unit = {
    val initializerStatements = new ArrayBuffer[Node]()
    new Root(program).transform(obj => obj.clazz match {
      case FieldWithInitializerKey => transformDeclarationWithInitializer(obj, initializerStatements, state)
      case _ =>
    })

    if (initializerStatements.isEmpty)
      return

    val reversedInitialiserStatements: ArrayBuffer[Node] = initializerStatements.reverse //TODO: hack to fix the reverse hack in NodeLike.

    val fieldInitializerMethod = MethodC.method(getFieldInitialiserMethodName,VoidTypeC.voidType, Seq.empty, reversedInitialiserStatements)
    val members: Seq[Node] = JavaClassSkeleton.getMembers(program)
    program(JavaClassSkeleton.Members) = Seq(fieldInitializerMethod) ++ members

    for(constructor <- members.filter(member => member.clazz == ConstructorC.ConstructorKey)) {
      val body = MethodC.getMethodBody(constructor)
      if (statementIsSuperCall(body.head)) {
        val bodyAfterHead = body.drop(1)
        val head = body.head
        val callToFieldInitialiser = ExpressionAsStatementC.asStatement(CallC.call(VariableC.variable(getFieldInitialiserMethodName)))
        constructor(MethodC.MethodBodyKey) = Seq(head, callToFieldInitialiser) ++ bodyAfterHead
      }
    }
  }

  def getFieldInitialiserMethodName: String = { //TODO make sure this name doesn't collide with other method names.
    "fieldInitialiser"
  }

  def statementIsSuperCall(statement: Node): Boolean = {
    statement.clazz == ExpressionAsStatementC.key &&
      ExpressionAsStatementC.getExpression(statement).clazz == SuperCallExpression.SuperCall
  }
}
