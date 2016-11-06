package transformations.bytecode.simpleBytecode

import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.Node
import core.particles.{CompilationState, Contract, ParticleWithGrammar, ParticleWithPhase}
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.additions.LabelledLocations.LabelKey
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.types.TypeSkeleton
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}

object InferredMaxStack extends ParticleWithPhase with ParticleWithGrammar {
  override def dependencies: Set[Contract] = Set(LabelledLocations)

  override def transform(program: Node, state: CompilationState): Unit = {
    val clazz = program

    def getMaxStack(method: Node): Integer = {
      val stackLayoutAnalysis = new InstructionTypeAnalysisFromState(state, method)

      val maxStack = stackLayoutAnalysis.typeStatePerInstruction.values.map(
        stackLayout => stackLayout.stackTypes.map(_type => TypeSkeleton.getTypeSize(_type,state)).sum).max
      maxStack
    }

    for (method <- ByteCodeSkeleton.getMethods(clazz)) {
      val code = ByteCodeMethodInfo.getMethodAttributes(method).find(a => a.clazz == CodeAttribute.CodeKey).get
      code(CodeAttribute.CodeMaxStackKey) = getMaxStack(method)
    }
  }

  def getInstructionStackSizeModification(constantPool: Seq[Any], instruction: Node): Integer = instruction.clazz match {
      case LabelKey => 0
  }

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    grammars.findPathsToKey(ProgramGrammar, CodeAttribute.MaxStackGrammar).head.removeMeFromSequence()
  }

  override def description: String = "Generates the code max stack value for code attributes which is required by the JVM."
}
