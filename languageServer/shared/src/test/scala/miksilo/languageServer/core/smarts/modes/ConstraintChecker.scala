package miksilo.languageServer.core.smarts.modes

import miksilo.languageServer.core.smarts.language.Program
import miksilo.languageServer.core.smarts.language.expressions.Expression
import miksilo.languageServer.core.smarts.language.types.LanguageType
import miksilo.languageServer.core.smarts.{ConstraintBuilder, ConstraintSolver, Factory}

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

  override def checkExpression(expression: Expression, languageType: LanguageType): Try[Unit] = {
    val factory = new Factory()
    val builder: ConstraintBuilder = new ConstraintBuilder(factory)
    builder.add(Program.libraryConstraints)
    val scope = factory.newScope()
    val _type = languageType.constraints(builder, scope)
    expression.constraints(builder, _type, scope)
    val constraints = builder.getConstraints
    new ConstraintSolver(builder, constraints).run()
  }
}
