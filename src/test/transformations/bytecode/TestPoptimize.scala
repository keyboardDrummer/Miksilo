package transformations.bytecode

import core.transformation.{MetaObject, CompilerFromTransformations}
import org.junit.{Assert, Test}
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.{IntegerConstantC, StoreIntegerC}
import transformations.bytecode.coreInstructions.PopC
import transformations.bytecode.additions.PoptimizeC
import transformations.javac.JavaCompiler
import transformations.javac.classes.ConstantPool

import scala.collection.mutable.ArrayBuffer

class TestPoptimize {

  @Test
  def testBasic() {
    val instructions = Seq(IntegerConstantC.integerConstant(3), PopC.pop)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(Seq.empty[MetaObject], newInstructions)
  }

  @Test
  def testMemory() {
    val instructions = Seq(IntegerConstantC.integerConstant(3),
      IntegerConstantC.integerConstant(2),
      PopC.pop,
      PopC.pop)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(Seq.empty[MetaObject], newInstructions)
  }


  @Test
  def testEncapsulation() {
    val expected = Seq(IntegerConstantC.integerConstant(2), StoreIntegerC.integerStore(0))
    val instructions = Seq(IntegerConstantC.integerConstant(3)) ++ expected ++ Seq(PopC.pop)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(expected, newInstructions)
  }


  @Test
  def testAdvanced() {
    val expected = Seq(IntegerConstantC.integerConstant(3),
      IntegerConstantC.integerConstant(3),
      StoreIntegerC.integerStore(0),
      StoreIntegerC.integerStore(0))
    val instructions = Seq(IntegerConstantC.integerConstant(3),
      IntegerConstantC.integerConstant(3),
      IntegerConstantC.integerConstant(3),
      PopC.pop,
      IntegerConstantC.integerConstant(3),
      IntegerConstantC.integerConstant(3),
      StoreIntegerC.integerStore(0),
      PopC.pop,
      StoreIntegerC.integerStore(0),
      PopC.pop)
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(expected, newInstructions)
  }

  @Test
  def testRobustness(){
    val instructions = Seq(IntegerConstantC.integerConstant(3))
    val newInstructions = transformInstructions(instructions)
    Assert.assertEquals(instructions, newInstructions)
  }

  def transformInstructions(instructions: Seq[MetaObject]) = {
    val codeAnnotation = CodeAttribute.codeAttribute(0, 0, 0, instructions, Seq(), Seq())
    val method = ByteCodeSkeleton.methodInfo(0, 0, Seq(codeAnnotation))
    val clazz = ByteCodeSkeleton.clazz(0, 0, new ConstantPool(), Seq(method))
    val compiler = new CompilerFromTransformations(Seq(PoptimizeC) ++ JavaCompiler.byteCodeTransformations)
    compiler.transform(clazz)
    CodeAttribute.getCodeInstructions(codeAnnotation)
  }
}
