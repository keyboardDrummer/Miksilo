package miksilo.modularLanguages.core

import miksilo.modularLanguages.core.deltas.path.{AnyPath, ChildPath, NodePath, PathRoot}
import miksilo.modularLanguages.core.deltas.{Contract, Delta, Property}
import miksilo.editorParser.LazyLogging
import miksilo.languageServer.core.language.exceptions.BadInputException
import miksilo.languageServer.core.language.{Compilation, Language, Phase}
import miksilo.languageServer.core.smarts.objects.NamedDeclaration
import miksilo.languageServer.core.smarts.{ConstraintBuilder, SolveException}
import miksilo.modularLanguages.core.node.TypedChildField
import miksilo.modularLanguages.deltas.ConstraintSkeleton

trait ConstraintCollector {
  def build(compilation: Compilation, builder: ConstraintBuilder): Unit
}

object SolveConstraintsDelta extends Delta with LazyLogging {

  val constraintCollector: Property[ConstraintCollector] = new Property[ConstraintCollector]((compilation, builder) => {
    ConstraintSkeleton.constraints(compilation, builder, compilation.program.asInstanceOf[PathRoot],
      builder.newScope(debugName = "rootScope"))
  })

  def getDeclarationOfReference(path: AnyPath): NodePath = resolvesToDeclaration(path).origin.get.asInstanceOf[ChildPath].parent
  val resolvesToDeclaration = new TypedChildField[NamedDeclaration]("resolvesToDeclaration")
  override def inject(language: Language): Unit = {
    super.inject(language)
    val phase = Language.getConstraintPhase((compilation, builder) => {
      constraintCollector.get(language).build(compilation, builder)
    })

    val withExtra = Phase(this, description, compilation => {
      phase.action(compilation)
      for(referenceWithDeclaration <- compilation.proofs.references) {
        referenceWithDeclaration._1.origin.foreach(ref => resolvesToDeclaration(ref.asInstanceOf[ChildPath]) = referenceWithDeclaration._2)
      }
    })
    language.compilerPhases ::= withExtra
  }

  case class ConstraintException(solveException: SolveException) extends BadInputException {
    override def toString: String = "Could not solve semantic constraints:" + solveException.toString
  }

  override def description: String = "Solves the semantic constraints"

  override def dependencies: Set[Contract] = Set.empty
}
