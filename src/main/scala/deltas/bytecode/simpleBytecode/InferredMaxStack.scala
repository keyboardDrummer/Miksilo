package deltas.bytecode.simpleBytecode

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.Node
import deltas.bytecode.ByteCodeMethodInfo.ByteCodeMethodInfoWrapper
import deltas.bytecode.ByteCodeSkeleton.ClassFile
import deltas.bytecode.additions.LabelledLocations
import deltas.bytecode.additions.LabelledLocations.LabelKey
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.types.TypeSkeleton

object InferredMaxStack extends DeltaWithPhase with DeltaWithGrammar {
  override def dependencies: Set[Contract] = Set(LabelledLocations)

  override def transformProgram(program: Node, state: Compilation): Unit = {
    val clazz: ClassFile[Node] = program

    def getMaxStack(method: ByteCodeMethodInfoWrapper[Node]): Int = {
      val stackLayoutAnalysis = new InstructionTypeAnalysisFromState(state, method)

      val maxStack = stackLayoutAnalysis.typeStatePerInstruction.values.map(
        stackLayout => stackLayout.stackTypes.map(_type => TypeSkeleton.getTypeSize(_type,state)).sum).max
      maxStack
    }

    for (method <- clazz.methods) {
      val code = method.codeAttribute
      code.maxStack = getMaxStack(method)
    }
  }

  def getInstructionStackSizeModification(constantPool: Seq[Any], instruction: Node): Integer = instruction.clazz match {
      case LabelKey => 0
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    grammars.find(CodeAttribute.CodeKey).findLabelled(CodeAttribute.MaxStackGrammar).removeMeFromSequence()
  }

  override def description: String = "Generates the code max stack value for code attributes which is required by the JVM."
}
