package deltas.verilog

import core.deltas.grammars.LanguageGrammars
import core.deltas.{Contract, DeltaWithGrammar}
import core.document.BlankLine
import core.language.Language
import core.language.node.{NodeField, NodeShape}

object PackageDelta extends DeltaWithGrammar {

  object Shape extends NodeShape
  object Members extends NodeField
  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    import grammars._
    val fileMember = find(VerilogFileDelta.Members)

    val packageMember = create(Members)
    val _package = "package" ~~ identifier ~ ";" %
      packageMember.manySeparatedVertical(BlankLine).as(Members) %
      "endpackage" ~~ (":" ~~ identifier).option
    fileMember.addAlternative(_package)

    val clazz = find(VerilogClassDelta.Shape)
    packageMember.addAlternative(clazz)
  }

  override def description: String = "Adds packages to Verilog"

  override def dependencies: Set[Contract] = Set(VerilogFileDelta, VerilogClassDelta)
}
