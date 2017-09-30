package transformations.bytecode.simpleBytecode

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import transformations.bytecode.ByteCodeMethodInfo.ByteCodeMethodInfoWrapper
import transformations.bytecode.ByteCodeSkeleton.ByteCodeWrapper
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.additions.LabelledLocations.LabelKey
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.types.TypeSkeleton

object InferredMaxStack extends DeltaWithPhase with DeltaWithGrammar {
  override def dependencies: Set[Contract] = Set(LabelledLocations)

  override def transform(program: Node, state: Compilation): Unit = {
    val clazz: ByteCodeWrapper[Node] = program

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

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    grammars.find(CodeAttribute.CodeKey).findLabelled(CodeAttribute.MaxStackGrammar).removeMeFromSequence()
  }

  override def description: String = "Generates the code max stack value for code attributes which is required by the JVM."
}
