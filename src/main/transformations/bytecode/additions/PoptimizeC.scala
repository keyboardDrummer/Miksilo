package transformations.bytecode.additions

import core.transformation.sillyCodePieces.ParticleWithPhase
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.PopC
import transformations.bytecode.ByteCodeSkeleton
import transformations.javac.classes.ConstantPool

object PoptimizeC extends ParticleWithPhase {


  override def dependencies: Set[Contract] = Set(PopC)

  override def transform(clazz: MetaObject, state: TransformationState): Unit = {
    val constantPool = ByteCodeSkeleton.getConstantPool(clazz)
    for (method <- ByteCodeSkeleton.getMethods(clazz)) {
      val codeAnnotation = ByteCodeSkeleton.getMethodAttributes(method).find(a => a.clazz == CodeAttribute.CodeKey).get
      val instructions = CodeAttribute.getCodeInstructions(codeAnnotation)

      val stackModRegistry = ByteCodeSkeleton.getInstructionSignatureRegistry(state)
      def getInOutSizes(instruction: MetaObject) = {
        val signature = stackModRegistry(instruction.clazz)(constantPool, instruction)
        val inCount = signature._1.size
        val outCount = signature._2.size
        (inCount, outCount)
      }

      var newInstructions = List.empty[MetaObject]
      var consumptions = List.empty[Boolean]
      def processInstruction(instruction: MetaObject) {
        if (instruction.clazz == PopC.PopKey) {
          consumptions ::= true
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
        val hasSideEffect = out == 0 //TODO dangerous assumption :D
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
}
