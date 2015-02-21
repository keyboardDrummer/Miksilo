package transformations.bytecode.additions

import core.transformation.sillyCodePieces.ParticleWithPhase
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions._

object PoptimizeC extends ParticleWithPhase {

  override def dependencies: Set[Contract] = Set(PopC)

  override def transform(clazz: MetaObject, state: TransformationState): Unit = {
    for (method <- ByteCodeSkeleton.getMethods(clazz)) {
      val codeAnnotation = ByteCodeSkeleton.getMethodAttributes(method).find(a => a.clazz == CodeAttribute.CodeKey).get
      val instructions = CodeAttribute.getCodeInstructions(codeAnnotation)

      def getInOutSizes(instruction: MetaObject) = InstructionC.getInOutSizes(instruction, state)

      var newInstructions = List.empty[MetaObject]
      var consumptions = List.empty[Boolean]
      def processInstruction(instruction: MetaObject) {
        if (instruction.clazz == PopC.PopKey) {
          consumptions ::= true
          return
        }

        if (instruction.clazz == Pop2C.Pop2Key) {
          consumptions = List(true,true) ++ consumptions
          return
        }

        val (in, out) = getInOutSizes(instruction)
        var outLeft = out
        var outPop = 0
        while (outLeft > 0 && consumptions.nonEmpty) {
          val pop = consumptions.head
          outPop += (if (pop) 1 else 0)
          consumptions = consumptions.tail
          outLeft -= 1
        }
        val outConsumption = out - outPop
        val hasSideEffect = guessIfInstructionHasSideEffect(out)
        val keepInstruction = outConsumption != 0 || hasSideEffect
        if (keepInstruction) {
          newInstructions = 0.until(outPop).map(_ => PopC.pop).toList ++ newInstructions
          consumptions = 0.until(in).map(_ => false).toList ++ consumptions
          newInstructions = instruction :: newInstructions
        }
      }

      for (instruction <- instructions.reverse) {
        processInstruction(instruction)
      }
      codeAnnotation(CodeAttribute.CodeInstructionsKey) = newInstructions.toSeq
    }
  }

  def guessIfInstructionHasSideEffect(out: Int): Boolean = {
    out == 0 //dangerous assumption :D
  }
}
