package transformations.javac.classes

import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.{Path, Root}
import core.particles.{CompilationState, Contract, ParticleWithGrammar, ParticleWithPhase}
import transformations.javac.methods.VariableC
import transformations.javac.methods.assignment.AssignmentSkeleton
import transformations.javac.statements.ExpressionAsStatementC
import transformations.javac.statements.locals.{LocalDeclarationC, LocalDeclarationWithInitializerC}

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
  }
}
