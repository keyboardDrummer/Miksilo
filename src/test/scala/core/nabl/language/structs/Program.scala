package core.nabl.language

import core.nabl.constraints.types.objects.{IntConstraintType, LongConstraintType}
import core.nabl.constraints.types.{AssignSubType, CheckSubType}
import core.nabl.constraints.{Constraint, ConstraintBuilder}
import core.nabl.language.modules.Module

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
