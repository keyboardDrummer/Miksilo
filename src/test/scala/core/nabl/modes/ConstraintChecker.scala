package core.nabl.modes

import core.nabl.{ConstraintBuilder, ConstraintSolver, Factory}
import core.nabl.Factory
import core.nabl.language.Program
import core.nabl.language.expressions.Expression
import core.nabl.language.types.LanguageType

trait ConstraintChecker extends Checker
{
  def subTyping: Boolean

  override def check(program: Program): Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory)
    program.constraints(builder)
    val constraints = builder.getConstraints
    new ConstraintSolver(builder, constraints).run()
  }

  override def checkExpression(expression: Expression, languageType: LanguageType): Boolean = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory)
    builder.add(Program.libraryConstraints)
    val scope = factory.newScope
    val _type = languageType.constraints(builder, scope)
    expression.constraints(builder, _type, scope)
    val constraints = builder.getConstraints
    new ConstraintSolver(builder, constraints).run()
  }
}
