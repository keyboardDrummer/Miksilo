package core.smarts

import com.typesafe.scalalogging.LazyLogging
import core.deltas.Delta
import core.language.exceptions.BadInputException
import core.language.{Language, Phase}

import scala.util.{Failure, Success}

object SolveConstraintsDelta extends Delta with LazyLogging {

  override def inject(language: Language): Unit = {
    super.inject(language)
    language.compilerPhases ::= Phase(this, compilation => {
      val factory = new Factory()
      val builder = new ConstraintBuilder(factory)
      language.collectConstraints(compilation, builder)

      val solver = builder.toSolver
      solver.run() match {
        case Success(_) =>
        case Failure(e:SolveException) =>
          logger.debug("Failed to solve constraints")
        case Failure(e) => throw e
      }
      compilation.proofs = solver.proofs
    })
  }

  case class ConstraintException(solveException: SolveException) extends BadInputException {
    override def toString: String = "Could not solve semantic constraints:" + solveException.toString
  }

  override def description: String = "Solves the semantic constraints"
}
