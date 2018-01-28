package core.nabl

import core.deltas.Delta
import core.deltas.exceptions.BadInputException
import core.language.{Language, Phase}

object SolveConstraints extends Delta {

  override def inject(language: Language): Unit = {
    super.inject(language)
    language.compilerPhases ::= Phase(this, compilation => {
      val factory = new Factory()
      val builder = new ConstraintBuilder(factory)
      language.collectConstraints(compilation, builder)

      val solver = new ConstraintSolver(builder, builder.getConstraints)
      val success = solver.run()
      if (!success)
        throw ConstraintException(solver)

      compilation.proofs = solver
    })
  }

  case class ConstraintException(solver: ConstraintSolver) extends BadInputException {
    override def toString = "Could not solve constraints"
  }

  override def description: String = "Adds the go to definition capability"
}
