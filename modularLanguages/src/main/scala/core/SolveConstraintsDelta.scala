package core

import com.typesafe.scalalogging.LazyLogging
import core.deltas.path.{AnyPath, ChildPath, NodePath, PathRoot}
import deltas.{Contract, Delta, Property}
import _root_.deltas.ConstraintSkeleton
import core.language.exceptions.BadInputException
import core.language.node.TypedChildField
import core.language.{Compilation, Language, Phase}
import core.smarts.objects.NamedDeclaration
import core.smarts.{ConstraintBuilder, CouldNotApplyConstraints, Factory, SolveException}

import scala.util.{Failure, Success}

trait ConstraintCollector {
  def build(compilation: Compilation, builder: ConstraintBuilder): Unit
}

object SolveConstraintsDelta extends Delta with LazyLogging {

  def constraintCollector: Property[ConstraintCollector] = new Property[ConstraintCollector]((compilation, builder) => {
    ConstraintSkeleton.constraints(compilation, builder, compilation.program,
      builder.newScope(debugName = "rootScope"))
  })

  def getDeclarationOfReference(path: AnyPath): NodePath = resolvesToDeclaration(path).origin.get.asInstanceOf[ChildPath].parent
  val resolvesToDeclaration = new TypedChildField[NamedDeclaration]("resolvesToDeclaration")
  override def inject(language: Language): Unit = {
    super.inject(language)
    language.compilerPhases ::= Phase(this, compilation => {
      val factory = new Factory()
      val builder = new ConstraintBuilder(factory)
      val start = System.currentTimeMillis()
      constraintCollector.get(language).build(compilation, builder)

      val solver = builder.toSolver

      solver.run() match {
        case Success(_) =>
          compilation.remainingConstraints = Seq.empty
        case Failure(e:CouldNotApplyConstraints) =>
          compilation.remainingConstraints = e.constraints
        case Failure(e:SolveException) =>
          throw ConstraintException(e)
        case Failure(e) => throw e
      }
      logger.info(s"Constraint solving took ${System.currentTimeMillis() - start}ms")

      for(refDecl <- solver.proofs.references) {
        refDecl._1.origin.foreach(ref => resolvesToDeclaration(ref.asInstanceOf[ChildPath]) = refDecl._2)
      }
      compilation.proofs = solver.proofs
      if (compilation.remainingConstraints.nonEmpty) {
        compilation.stopped = true
      }
      compilation.diagnostics ++= compilation.remainingConstraints.flatMap(
        constraint => constraint.getDiagnostic)
    })
  }

  case class ConstraintException(solveException: SolveException) extends BadInputException {
    override def toString: String = "Could not solve semantic constraints:" + solveException.toString
  }

  override def description: String = "Solves the semantic constraints"

  override def dependencies: Set[Contract] = Set.empty
}
