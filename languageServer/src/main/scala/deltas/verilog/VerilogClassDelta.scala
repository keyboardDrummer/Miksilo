package deltas.verilog

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node.{NodeLike, NodeShape, NodeWrapper}
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.HasNameDelta.HasName
import deltas.javac.classes.skeleton.HasConstraintsDelta

object VerilogClassDelta extends DeltaWithGrammar with HasConstraintsDelta {
  override def description: String = "Adds Verilog classes"

  import deltas.HasNameDelta.Name
  object Shape extends NodeShape

  implicit class Class[T <: NodeLike](val node: T) extends NodeWrapper[T] with HasName[T] {
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val clazz = ("class" ~~ find(Name) ~ ";" % "endclass" ~~ (":" ~~ identifier).option) asNode Shape
    create(Shape, clazz)
  }


  override def dependencies: Set[Contract] = Set.empty

  override def shape: NodeShape = Shape

  override def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    val _clazz: Class[NodePath] = path
    builder.declare(_clazz.name, parentScope, _clazz.getField(Name))
  }
}
