package deltas.bytecode.simpleBytecode

import core.bigrammar.grammars.IgnoreLeft
import core.bigrammar.{GrammarReference, RootGrammar}
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas.path.{Path, PathRoot}
import core.deltas.{Compilation, DeltaWithGrammar, DeltaWithPhase, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.ByteCodeSkeleton.{ClassFile, ConstantPoolGrammar}
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar
import deltas.javac.classes.ConstantPool

object InlineConstantPool extends DeltaWithPhase with DeltaWithGrammar {

  override def transform(program: Node, state: Compilation): Unit = {
    val constantPool = new ConstantPool()
    program.constantPool = constantPool
    val constantReferences = ByteCodeSkeleton.getRegistry(state).constantReferences

    PathRoot(program).visit(afterChildren = extractReferencesInNode)

    def extractReferencesInNode(node: Path): Unit = {
      for {
        references <- constantReferences.get(node.clazz)
        reference <- references
        fieldValue <- node.current.get(reference._1)
      } {
        val index = constantPool.store(fieldValue)
        node.current.data.put(reference._1, index)
      }
    }
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    inlineConstantPoolReferences(language)
    simplifyConstantEntryGrammars(language)
    removeConstantPoolGrammar(language)
  }

  private def inlineConstantPoolReferences(language: Language): Unit = {
    val grammars = language.grammars
    val constantReferences = ByteCodeSkeleton.getRegistry(language).constantReferences
    val constantPoolIndexGrammar = grammars.find(ConstantPoolIndexGrammar)
    for (containerEntry <- constantReferences) {
      val key: GrammarKey = containerEntry._1
      val constantFields: Map[NodeField, NodeClass] = containerEntry._2
      val keyGrammar = new RootGrammar(grammars.find(key))
      for (field <- constantFields) {
        val asGrammar = keyGrammar.findAs(field._1)
        val constantRef = asGrammar.findGrammar(constantPoolIndexGrammar).get.asInstanceOf[GrammarReference]
        constantRef.set(grammars.find(field._2))
      }
    }
  }

  private def simplifyConstantEntryGrammars(language: Language): Unit = {
    val grammars = language.grammars
    for (entry <- ByteCodeSkeleton.getRegistry(language).constantEntries) {
      val ignoreLeft = grammars.find(entry.key).find(p => p.value.isInstanceOf[IgnoreLeft]).get
      ignoreLeft.set(ignoreLeft.value.asInstanceOf[IgnoreLeft].sequence.second)
    }
  }

  private def removeConstantPoolGrammar(language: Language): Unit = {
    val constantPoolGrammar = language.grammars.root.findLabelled(ConstantPoolGrammar)
    constantPoolGrammar.previous.asInstanceOf[GrammarReference].removeMeFromSequence()
  }

  override def description: String = "Removes the constant pool in favor of inline constant entries"
}