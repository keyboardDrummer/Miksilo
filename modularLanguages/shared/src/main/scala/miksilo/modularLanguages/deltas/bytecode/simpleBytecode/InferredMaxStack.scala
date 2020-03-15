package miksilo.modularLanguages.deltas.bytecode.simpleBytecode

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.ByteCodeMethodInfo.MethodInfo
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton.ClassFile
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.types.TypeSkeleton

object InferredMaxStack extends DeltaWithPhase with DeltaWithGrammar {
  override def dependencies: Set[Contract] = Set(LabelledLocations)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val classFile: ClassFile[Node] = program

    def getMaxStack(method: MethodInfo[Node]): Int = {
      val stackLayoutAnalysis = new InstructionTypeAnalysisForMethod(program, compilation, method)

      val maxStack = stackLayoutAnalysis.typeStatePerInstruction.values.map(
        stackLayout => stackLayout.stackTypes.map(_type => TypeSkeleton.getTypeSize(_type,compilation)).sum).max
      maxStack
    }

    for (method <- classFile.methods) {
      val code = method.codeAttribute
      code.maxStack = getMaxStack(method)
    }
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    find(CodeAttributeDelta.CodeKey).findLabelled(CodeAttributeDelta.MaxStackGrammar).removeMe()
  }

  override def description: String = "Generates the code max stack value for code attributes which is required by the JVM."
}
