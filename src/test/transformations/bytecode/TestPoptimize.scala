package transformations.bytecode

import core.particles.CompilerFromParticles
import core.particles.node.Node
import org.junit.{Assert, Test}
import transformations.bytecode.additions.PoptimizeC
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.{SmallIntegerConstantC, StoreIntegerC}
import transformations.bytecode.coreInstructions.longs.PushLongC
import transformations.bytecode.coreInstructions.{Pop2C, PopC, VoidReturnInstructionC}
import transformations.javac.JavaCompiler
import transformations.javac.classes.ConstantPool
import transformations.bytecode.types.VoidTypeC
import transformations.javac.types.MethodTypeC

class TestPoptimize {

  @Test
  def testBasic() {
    val instructions = Seq(SmallIntegerConstantC.integerConstant(3), PopC.pop, VoidReturnInstructionC.voidReturn)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(Seq(VoidReturnInstructionC.voidReturn), newInstructions)
  }

  @Test
  def testMemory() {
    val instructions = Seq(SmallIntegerConstantC.integerConstant(3),
      SmallIntegerConstantC.integerConstant(2),
      PopC.pop,
      PopC.pop,
      VoidReturnInstructionC.voidReturn)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(Seq(VoidReturnInstructionC.voidReturn), newInstructions)
  }


  @Test
  def testEncapsulation() {
    val middle = Seq(SmallIntegerConstantC.integerConstant(2), StoreIntegerC.integerStore(0))
    val expected = middle ++ Seq(VoidReturnInstructionC.voidReturn)
    val instructions = Seq(SmallIntegerConstantC.integerConstant(3)) ++ middle ++ Seq(PopC.pop, VoidReturnInstructionC.voidReturn)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(expected, newInstructions)
  }

  @Test
  def testAdvanced() {
    val expected = Seq(SmallIntegerConstantC.integerConstant(3),
      SmallIntegerConstantC.integerConstant(3),
      StoreIntegerC.integerStore(0),
      StoreIntegerC.integerStore(0),
      VoidReturnInstructionC.voidReturn)
    val instructions = Seq(SmallIntegerConstantC.integerConstant(3),
      SmallIntegerConstantC.integerConstant(3),
      SmallIntegerConstantC.integerConstant(3),
      PopC.pop,
      SmallIntegerConstantC.integerConstant(3),
      SmallIntegerConstantC.integerConstant(3),
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
    val instructions = Seq(SmallIntegerConstantC.integerConstant(3), VoidReturnInstructionC.voidReturn)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(instructions, newInstructions)
  }

  def transformInstructions(instructions: Seq[Node]) = {
    val codeAnnotation = CodeAttribute.codeAttribute(0, 0, 0, instructions, Seq(), Seq())
    val method = ByteCodeMethodInfo.methodInfo(0, 1, Seq(codeAnnotation))
    method(ByteCodeMethodInfo.AccessFlagsKey) = Set(ByteCodeMethodInfo.StaticAccess)
    val clazz = ByteCodeSkeleton.clazz(0, 0, new ConstantPool(Seq(MethodTypeC.construct(VoidTypeC.voidType,Seq.empty))), Seq(method))
    val compiler = new CompilerFromParticles(Seq(PoptimizeC) ++ JavaCompiler.byteCodeTransformations)
    compiler.transform(clazz)
    CodeAttribute.getCodeInstructions(codeAnnotation)
  }
  
  @Test
  def testPop2() = {
    val instructions = Seq(PushLongC.constant(1), Pop2C.pop2, VoidReturnInstructionC.voidReturn)
    val expected = Seq(VoidReturnInstructionC.voidReturn)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(expected, newInstructions)
  }
}
