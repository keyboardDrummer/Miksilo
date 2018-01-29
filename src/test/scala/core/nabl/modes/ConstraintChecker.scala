package core.nabl.modes

import core.nabl.language.Program
import core.nabl.language.expressions.Expression
import core.nabl.language.types.LanguageType
import core.nabl.{ConstraintBuilder, ConstraintSolver, Factory}

import scala.util.Try

trait ConstraintChecker extends Checker
{
  def subTyping: Boolean

  def succeed(program: Program): Unit = {
    run(program).get
  }

  override def check(program: Program): Boolean = {
    val result: Try[Unit] = run(program)
    result.isSuccess
  }

  private def run(program: Program) = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory)
    program.constraints(builder)
    val constraints = builder.getConstraints
    val result = new ConstraintSolver(builder, constraints).run()
    result
  }

  override def checkExpression(expression: Expression, languageType: LanguageType): Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory)
    builder.add(Program.libraryConstraints)
    val scope = factory.newScope
    val _type = languageType.constraints(builder, scope)
    expression.constraints(builder, _type, scope)
    val constraints = builder.getConstraints
    new ConstraintSolver(builder, constraints).run().isSuccess
  }
}
