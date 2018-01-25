package core.nabl.language

import core.nabl.language.modules.Module
import core.nabl.types.AssignSubType
import core.nabl.types.objects.{IntConstraintType, LongConstraintType}
import core.nabl.{Constraint, ConstraintBuilder}

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
