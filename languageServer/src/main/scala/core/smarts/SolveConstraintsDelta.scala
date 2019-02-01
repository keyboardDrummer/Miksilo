package core.smarts

import com.typesafe.scalalogging.LazyLogging
import core.deltas.path.{AnyPath, ChildPath, NodePath}
import core.deltas.{Contract, Delta}
import core.language.exceptions.BadInputException
import core.language.node.TypedChildField
import core.language.{Language, Phase}
import core.smarts.objects.NamedDeclaration

import scala.util.{Failure, Success}

object SolveConstraintsDelta extends Delta with LazyLogging {

  def injectPhaseAfterMe(language: Language, phase: Phase): Unit = {
    val solvePhaseIndex = language.compilerPhases.indexWhere(p => p.key == SolveConstraintsDelta)
    val (left, right) = language.compilerPhases.splitAt(solvePhaseIndex + 1)
    language.compilerPhases = left ++ (phase :: right)
  }

  def getDeclarationOfReference(path: AnyPath): NodePath = resolvesToDeclaration(path).origin.get.asInstanceOf[ChildPath].parent
  val resolvesToDeclaration = new TypedChildField[NamedDeclaration]("resolvesToDeclaration")
  override def inject(language: Language): Unit = {
    super.inject(language)
    language.compilerPhases ::= Phase(this, compilation => {
      val factory = new Factory()
      val builder = new ConstraintBuilder(factory)
      language.collectConstraints(compilation, builder)

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

      for(refDecl <- solver.proofs.declarations) {
        refDecl._1.origin.foreach(ref => resolvesToDeclaration(ref.asInstanceOf[ChildPath]) = refDecl._2)
      }
      compilation.proofs = solver.proofs
      if (compilation.remainingConstraints.nonEmpty) {
        compilation.stopped = true
      }
      compilation.diagnostics ++= compilation.remainingConstraints.flatMap(
        constraint => constraint.getDiagnostic.toSeq)
    })
  }

  case class ConstraintException(solveException: SolveException) extends BadInputException {
    override def toString: String = "Could not solve semantic constraints:" + solveException.toString
  }

  override def description: String = "Solves the semantic constraints"

  override def dependencies: Set[Contract] = Set.empty
}
