package transformations.bytecode

import core.transformation.{CompilerFromParticles, MetaObject}
import org.junit.{Assert, Test}
import transformations.bytecode.additions.PoptimizeC
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.longs.LongConstantC
import transformations.bytecode.coreInstructions.{Pop2C, VoidReturnInstructionC, PopC}
import transformations.bytecode.coreInstructions.integers.{IntegerConstantC, StoreIntegerC}
import transformations.javac.JavaCompiler
import transformations.javac.classes.ConstantPool

class TestPoptimize {

  @Test
  def testBasic() {
    val instructions = Seq(IntegerConstantC.integerConstant(3), PopC.pop, VoidReturnInstructionC.voidReturn)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(Seq(VoidReturnInstructionC.voidReturn), newInstructions)
  }

  @Test
  def testMemory() {
    val instructions = Seq(IntegerConstantC.integerConstant(3),
      IntegerConstantC.integerConstant(2),
      PopC.pop,
      PopC.pop,
      VoidReturnInstructionC.voidReturn)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(Seq(VoidReturnInstructionC.voidReturn), newInstructions)
  }


  @Test
  def testEncapsulation() {
    val middle = Seq(IntegerConstantC.integerConstant(2), StoreIntegerC.integerStore(0))
    val expected = middle ++ Seq(VoidReturnInstructionC.voidReturn)
    val instructions = Seq(IntegerConstantC.integerConstant(3)) ++ middle ++ Seq(PopC.pop, VoidReturnInstructionC.voidReturn)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(expected, newInstructions)
  }

  @Test
  def testAdvanced() {
    val expected = Seq(IntegerConstantC.integerConstant(3),
      IntegerConstantC.integerConstant(3),
      StoreIntegerC.integerStore(0),
      StoreIntegerC.integerStore(0),
      VoidReturnInstructionC.voidReturn)
    val instructions = Seq(IntegerConstantC.integerConstant(3),
      IntegerConstantC.integerConstant(3),
      IntegerConstantC.integerConstant(3),
      PopC.pop,
      IntegerConstantC.integerConstant(3),
      IntegerConstantC.integerConstant(3),
      StoreIntegerC.integerStore(0),
      PopC.pop,
      StoreIntegerC.integerStore(0),
      PopC.pop,
      VoidReturnInstructionC.voidReturn)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(expected, newInstructions)
  }

  @Test
  def testRobustness(){
    val instructions = Seq(IntegerConstantC.integerConstant(3), VoidReturnInstructionC.voidReturn)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(instructions, newInstructions)
  }

  def transformInstructions(instructions: Seq[MetaObject]) = {
    val codeAnnotation = CodeAttribute.codeAttribute(0, 0, 0, instructions, Seq(), Seq())
    val method = ByteCodeSkeleton.methodInfo(0, 0, Seq(codeAnnotation))
    val clazz = ByteCodeSkeleton.clazz(0, 0, new ConstantPool(), Seq(method))
    val compiler = new CompilerFromParticles(Seq(PoptimizeC) ++ JavaCompiler.byteCodeTransformations)
    compiler.transform(clazz)
    CodeAttribute.getCodeInstructions(codeAnnotation)
  }
  
  @Test
  def testPop2() = {
    val instructions = Seq(LongConstantC.constant(1), Pop2C.pop2)
    val expected = Seq.empty[MetaObject]
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(expected, newInstructions)
  }
}
