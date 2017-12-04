package deltas.bytecode.simpleBytecode

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.Node
import deltas.bytecode.ByteCodeMethodInfo.MethodInfo
import deltas.bytecode.ByteCodeSkeleton.ClassFile
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.types.TypeSkeleton

object InferredMaxStack extends DeltaWithPhase with DeltaWithGrammar {
  override def dependencies: Set[Contract] = Set(LabelledLocations)

  override def transformProgram(program: Node, compilation: Compilation): Unit = {
    val clazz: ClassFile[Node] = program

    def getMaxStack(method: MethodInfo[Node]): Int = {
      val stackLayoutAnalysis = new InstructionTypeAnalysisForMethod(program, compilation, method)

      val maxStack = stackLayoutAnalysis.typeStatePerInstruction.values.map(
        stackLayout => stackLayout.stackTypes.map(_type => TypeSkeleton.getTypeSize(_type,compilation)).sum).max
      maxStack
    }

    for (method <- clazz.methods) {
      val code = method.codeAttribute
      code.maxStack = getMaxStack(method)
    }
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.find(CodeAttributeDelta.CodeKey).findLabelled(CodeAttributeDelta.MaxStackGrammar).removeMe()
  }

  override def description: String = "Generates the code max stack value for code attributes which is required by the JVM."
}
