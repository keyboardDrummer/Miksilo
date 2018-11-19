package deltas.bytecode.simpleBytecode

import core.bigrammar.grammars.Labelled
import core.bigrammar.{BiGrammar, GrammarReference}
import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.language.node._
import core.language.{Compilation, Language}
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
    val fieldConstantTypesPerClass = ByteCodeSkeleton.constantReferences.get(compilation)

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
    simplifyConstantEntryGrammars(language)
    inlineConstantPoolReferences(language)
    removeConstantPoolGrammar(language)
  }

  private def inlineConstantPoolReferences(language: Language): Unit = {
    import language.grammars._
    val constantReferences = ByteCodeSkeleton.constantReferences.get(language)
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
    for (entry <- ByteCodeSkeleton.constantEntries.get(language).values) {
      val entryGrammar = grammars.find(entry.shape).find(g => g.value.isInstanceOf[NodeGrammar]).get.value.asInstanceOf[NodeGrammar]
      entryGrammar.inner = entry.getConstantEntryGrammar(grammars)
    }
  }

  private def removeConstantPoolGrammar(language: Language): Unit = {
    val root: Labelled = language.grammars.root
    val constantPoolGrammar: GrammarReference = root.findLabelled(ConstantPoolGrammar)
    constantPoolGrammar.removeMe()
  }
}