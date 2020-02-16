package core.smarts.language

import core.smarts.language.modules.Module
import core.smarts.types.AssignSubType
import core.smarts.types.objects.{IntConstraintType, LongConstraintType}
import core.smarts.{Constraint, ConstraintBuilder}

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
