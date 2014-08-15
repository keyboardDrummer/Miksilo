package transformations.bytecode

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton.CodeInstructionsKey
import transformations.bytecode.coreInstructions.PopC
import transformations.javac.classes.ConstantPool
import transformations.types.TypeC

object PoptimizeC extends ProgramTransformation {


  override def dependencies: Set[Contract] = Set(PopC)

  override def transform(clazz: MetaObject, state: TransformationState): Unit = {
    val constantPool = new ConstantPool(ByteCodeSkeleton.getConstantPool(clazz))
    for (method <- ByteCodeSkeleton.getMethods(clazz)) {
      val codeAnnotation = ByteCodeSkeleton.getMethodAttributes(method).find(a => a.clazz == ByteCodeSkeleton.CodeKey).get
      val instructions = ByteCodeSkeleton.getCodeInstructions(codeAnnotation)

      val stackModRegistry = ByteCodeSkeleton.getInstructionSignatureRegistry(state)
      def getInOutSizes(instruction: MetaObject) = {
        val signature = stackModRegistry(instruction.clazz)(constantPool, instruction)
        val inCount = signature._1.map(t => TypeC.getTypeSize(t, state)).sum
        val outCount = signature._2.map(t => TypeC.getTypeSize(t, state)).sum
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
      codeAnnotation(CodeInstructionsKey) = newInstructions.toSeq
    }
  }
}
