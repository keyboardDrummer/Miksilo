package miksilo.languageServer.core.smarts.language

import miksilo.languageServer.core.smarts.language.modules.Module
import miksilo.languageServer.core.smarts.types.AssignSubType
import miksilo.languageServer.core.smarts.types.objects.{IntConstraintType, LongConstraintType}
import miksilo.languageServer.core.smarts.{Constraint, ConstraintBuilder}

object Program
{
  def libraryConstraints: List[Constraint] = {
    List(AssignSubType(IntConstraintType, LongConstraintType))
  }
}

case class Program(modules: Seq[Module])
{
  def constraints(builder: ConstraintBuilder): Unit = {
    builder.add(Program.libraryConstraints)
    val scope = builder.newScope()
    modules.foreach(module => module.constraints(builder, scope))
  }
}
