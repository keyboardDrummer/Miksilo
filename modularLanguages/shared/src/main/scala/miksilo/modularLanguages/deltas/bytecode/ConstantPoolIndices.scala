package miksilo.modularLanguages.deltas.bytecode

import miksilo.modularLanguages.core.bigrammar.grammars.ManyVertical
import miksilo.modularLanguages.core.bigrammar.{BiGrammar, GrammarReference, RootGrammar}
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.{Node, NodeField, NodeShape}
import miksilo.modularLanguages.core.deltas.{Contract, DeltaWithGrammar}
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton.{ConstantPoolGrammar, ConstantPoolItemContentGrammar}

object ConstantPoolIndices extends DeltaWithGrammar {

  override def description: String = "Add indices to the constant pool entries"

  override def dependencies: Set[Contract] = Set[Contract](ByteCodeSkeleton)

  private object WithIndexClass extends NodeShape

  private object Index extends NodeField

  private object Content extends NodeField

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val previousConstantPoolItem = find(ByteCodeSkeleton.ConstantPoolItemContentGrammar)
    val constantPoolItem = (("#" ~> number.as(Index) ~~< "=") ~~ previousConstantPoolItem.inner.as(Content)).
      asNode(WithIndexClass)
    previousConstantPoolItem.inner = constantPoolItem

    val constantPoolGrammar = find(ConstantPoolGrammar)
    val entries: GrammarReference = new RootGrammar(constantPoolGrammar).findLabelled(ConstantPoolItemContentGrammar).ancestors.filter(p => p.value.isInstanceOf[ManyVertical]).head.asInstanceOf[GrammarReference]
    entries.set(addIndicesToList(entries.value))
  }

  def addIndicesToList(listGrammar: BiGrammar): BiGrammar = {
    val removeIndexForParsing: Seq[Node] => Seq[Any] = items => items.map(i => i(Content))
    val addIndexForPrinting: Seq[Any] => Seq[Node] = items => items.zipWithIndex.map((p: (Any, Int)) =>
      WithIndexClass.create(Index -> (p._2 + 1), Content -> p._1))
    listGrammar.map[Seq[Node], Seq[Any]](removeIndexForParsing, addIndexForPrinting)
  }
}
