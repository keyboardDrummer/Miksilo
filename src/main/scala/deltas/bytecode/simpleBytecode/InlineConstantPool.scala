package deltas.bytecode.simpleBytecode

import core.bigrammar.{BiGrammar, GrammarReference}
import core.bigrammar.grammars.{IgnoreLeft, Labelled}
import core.deltas.grammars.LanguageGrammars
import core.deltas.node._
import core.deltas._
import core.language.Language
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.ByteCodeSkeleton.{ClassFile, ConstantPoolGrammar}
import deltas.bytecode.coreInstructions.ConstantPoolIndexGrammar
import deltas.javac.classes.ConstantPool

object InlineConstantPool extends DeltaWithPhase with DeltaWithGrammar {

  override def description: String = "Removes the constant pool in favor of inline constant entries"

  override def dependencies: Set[Contract] = Set[Contract](ByteCodeSkeleton)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val constantPool = new ConstantPool()
    program.constantPool = constantPool
    val fieldConstantTypesPerClass = ByteCodeSkeleton.getRegistry(compilation).constantReferences

    program.visit(afterChildren = extractReferencesInNode)

    def extractReferencesInNode(node: Node): Unit = {
      for {
        fieldConstantTypes: Map[NodeField, NodeShape] <- fieldConstantTypesPerClass.get(node.shape)
        field <- fieldConstantTypes.keys
        constantPoolElement <- node.get(field)
      } {
        val index = constantPool.store(constantPoolElement)
        node.data.put(field, index)
      }
    }
  }

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    inlineConstantPoolReferences(language)
    simplifyConstantEntryGrammars(language)
    removeConstantPoolGrammar(language)
  }

  private def inlineConstantPoolReferences(language: Language): Unit = {
    import language.grammars._
    val constantReferences = ByteCodeSkeleton.getRegistry(language).constantReferences
    val constantPoolIndexGrammar = find(ConstantPoolIndexGrammar)
    for (classWithConstantReferences <- constantReferences) {
      val shape: NodeShape = classWithConstantReferences._1
      val constantReferences: Map[NodeField, NodeShape] = classWithConstantReferences._2
      val classGrammar: BiGrammar = find(shape)
      for (constantReference <- constantReferences) {
        val field = constantReference._1
        val constantType = constantReference._2
        val fieldGrammar: GrammarReference = classGrammar.findAs(field)
        val constantReferenceGrammar: GrammarReference = fieldGrammar.findGrammar(constantPoolIndexGrammar).get
        val constantElementGrammar: BiGrammar = find(constantType)
        constantReferenceGrammar.set(constantElementGrammar)
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
    val root: Labelled = language.grammars.root
    val constantPoolGrammar: GrammarReference = root.findLabelled(ConstantPoolGrammar)
    constantPoolGrammar.removeMe()
  }
}