package deltas.verilog

import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object VerilogClassDelta extends DeltaWithGrammar {
  override def description: String = "Adds Verilog classes"

  object Shape extends NodeShape
  object Name extends NodeField
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._

    val clazz = ("class" ~~ identifier.as(Name) ~ ";" % "endclass" ~~ (":" ~~ identifier).option) asNode(Shape)
    create(Shape, clazz)
  }


  override def dependencies: Set[Contract] = Set.empty
}
