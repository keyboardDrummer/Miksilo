package transformations.bytecode

import core.transformation.MetaObject
import org.junit.{Assert, Test}
import transformations.bytecode.coreInstructions._
import transformations.bytecode.coreInstructions.integerCompare.IfIntegerCompareGreaterOrEqualC
import transformations.javac.classes.QualifiedClassName
import transformations.javac.{ConstructorC, TestUtils}
import transformations.types.{ArrayTypeC, IntTypeC, ObjectTypeC, VoidTypeC}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TestPrintByteCodeFibonacciWithMain {

  val className = "Fibonacci"
  val fibonacciMethodName = "fibonacci"

  @Test
  def testPrintByteCode() = {
    val expectedHex = "cafe babe 0000 0033 0020 0a00 0600 1209\n0013 0014 0a00 0500 150a 0016 0017 0700\n1807 0019 0100 063c 696e 6974 3e01 0003\n2829 5601 0004 436f 6465 0100 0f4c 696e\n654e 756d 6265 7254 6162 6c65 0100 046d\n6169 6e01 0016 285b 4c6a 6176 612f 6c61\n6e67 2f53 7472 696e 673b 2956 0100 0966\n6962 6f6e 6163 6369 0100 0428 4929 4901\n000d 5374 6163 6b4d 6170 5461 626c 6501\n000a 536f 7572 6365 4669 6c65 0100 0e46\n6962 6f6e 6163 6369 2e6a 6176 610c 0007\n0008 0700 1a0c 001b 001c 0c00 0d00 0e07\n001d 0c00 1e00 1f01 0009 4669 626f 6e61\n6363 6901 0010 6a61 7661 2f6c 616e 672f\n4f62 6a65 6374 0100 106a 6176 612f 6c61\n6e67 2f53 7973 7465 6d01 0003 6f75 7401\n0015 4c6a 6176 612f 696f 2f50 7269 6e74\n5374 7265 616d 3b01 0013 6a61 7661 2f69\n6f2f 5072 696e 7453 7472 6561 6d01 0005\n7072 696e 7401 0004 2849 2956 0020 0005\n0006 0000 0000 0003 0000 0007 0008 0001\n0009 0000 001d 0001 0001 0000 0005 2ab7\n0001 b100 0000 0100 0a00 0000 0600 0100\n0000 0100 0900 0b00 0c00 0100 0900 0000\n2700 0200 0100 0000 0bb2 0002 08b8 0003\nb600 04b1 0000 0001 000a 0000 000a 0002\n0000 0004 000a 0005 0009 000d 000e 0001\n0009 0000 003a 0003 0001 0000 0017 1a05\na200 0704 a700 101a 0464 b800 031a 0564\nb800 0360 ac00 0000 0200 0a00 0000 0600\n0100 0000 0800 0f00 0000 0500 0209 4c01\n0001 0010 0000 0002 0011"
    Assert.assertEquals(expectedHex, TestUtils.printByteCode(getByteCode))
  }

  def getByteCode: MetaObject = {
    val classAttributes = Seq(ByteCodeSkeleton.sourceFile(16, 17))
    ByteCodeSkeleton.clazz(5, 6, getConstantPool, Seq[MetaObject](getConstructorByteCode, getMainByteCode, getFibonacciMethod), attributes = classAttributes)
  }

  @Test
  def runCompiledFibonacci() {
    val code = getByteCode
    val expectedResult = 8
    TestUtils.runByteCode(className, code, expectedResult)
  }

  def getExpectedUnoptimizedFibonacciWithoutMainByteCode: MetaObject = {
    val constantPool: mutable.Buffer[Any] = getConstantPool
    val method: MetaObject = getFibonacciMethod
    val nativeClass = ByteCodeSkeleton.clazz(3, 4, constantPool, Seq(getConstructorByteCode, getMainByteCode, method))
    nativeClass
  }

  def getFibonacciMethod: MetaObject = {
    val instructions = Seq(
      LoadIntegerC.integerLoad(0),
      IntegerConstantC.integerConstant(2),
      IfIntegerCompareGreaterOrEqualC.ifIntegerCompareGreater(7),
      IntegerConstantC.integerConstant(1),
      GotoC.goTo(16),
      LoadIntegerC.integerLoad(0),
      IntegerConstantC.integerConstant(1),
      SubtractIntegerC.subtractInteger,
      InvokeStaticC.invokeStatic(3),
      LoadIntegerC.integerLoad(0),
      IntegerConstantC.integerConstant(2),
      SubtractIntegerC.subtractInteger,
      InvokeStaticC.invokeStatic(3),
      AddIntegersC.addInteger,
      IntegerReturnInstructionC.integerReturn
    )
    val lineNumberTable = ByteCodeSkeleton.lineNumberTable(10, Seq(new LineNumberRef(8, 0)))
    val stackMapTable = ByteCodeSkeleton.stackMapTable(15, Seq(ByteCodeSkeleton.sameFrame(9),
      ByteCodeSkeleton.sameFrameLocals1StackItem(12, IntTypeC.intType)))
    val method = ByteCodeSkeleton.methodInfo(13, 14, Seq(ByteCodeSkeleton.codeAttribute(9, 3, 1, instructions, Seq(), Seq(lineNumberTable, stackMapTable))),
      Set(ByteCodeSkeleton.PublicAccess, ByteCodeSkeleton.StaticAccess))
    method
  }

  def getConstantPool: mutable.Buffer[Any] = {
    val constantPool = ArrayBuffer[Any](ByteCodeSkeleton.methodRef(6, 18),
      ByteCodeSkeleton.fieldRef(19, 20),
      ByteCodeSkeleton.methodRef(5, 21),
      ByteCodeSkeleton.methodRef(22, 23),
      ByteCodeSkeleton.classRef(24),
      ByteCodeSkeleton.classRef(25),
      ConstructorC.constructorName,
      ByteCodeSkeleton.methodDescriptor(VoidTypeC.voidType, Seq()),
      ByteCodeSkeleton.CodeAttributeId,
      ByteCodeSkeleton.LineNumberTableId,
      "main",
      ByteCodeSkeleton.methodDescriptor(VoidTypeC.voidType, Seq(
        ArrayTypeC.arrayType(ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "lang", "String")))))),
      fibonacciMethodName,
      ByteCodeSkeleton.methodDescriptor(IntTypeC.intType, Seq(IntTypeC.intType)),
      ByteCodeSkeleton.StackMapTableId,
      ByteCodeSkeleton.SourceFileId,
      "Fibonacci.java",
      ByteCodeSkeleton.nameAndType(7, 8),
      ByteCodeSkeleton.classRef(26),
      ByteCodeSkeleton.nameAndType(27, 28),
      ByteCodeSkeleton.nameAndType(13, 14),
      ByteCodeSkeleton.classRef(29),
      ByteCodeSkeleton.nameAndType(30, 31),
      "Fibonacci",
      new QualifiedClassName(Seq("java", "lang", "Object")),
      new QualifiedClassName(Seq("java", "lang", "System")),
      "out",
      ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "io", "PrintStream"))),
      "java/io/PrintStream",
      "print",
      ByteCodeSkeleton.methodDescriptor(VoidTypeC.voidType, Seq(IntTypeC.intType))
    )
    constantPool
  }

  def getConstructorByteCode: MetaObject = {
    val instructions = Seq(LoadAddressC.addressLoad(0),
      InvokeSpecialC.invokeSpecial(1), VoidReturnInstructionC.voidReturn)
    val lineNumberTable = ByteCodeSkeleton.lineNumberTable(10, Seq(new LineNumberRef(1, 0)))
    val codeAttribute = Seq(ByteCodeSkeleton.codeAttribute(9, 1, 1, instructions, Seq(), Seq(lineNumberTable)))
    ByteCodeSkeleton.methodInfo(7, 8, codeAttribute, Set())
  }

  def getMainByteCode: MetaObject = {
    val instructions = Seq(GetStaticC.getStatic(2),
      IntegerConstantC.integerConstant(5),
      InvokeStaticC.invokeStatic(3),
      InvokeVirtualC.invokeVirtual(4),
      VoidReturnInstructionC.voidReturn)
    val lineNumberTable = ByteCodeSkeleton.lineNumberTable(10, Seq(new LineNumberRef(4, 0), new LineNumberRef(5, 10)))
    ByteCodeSkeleton.methodInfo(11, 12, Seq(ByteCodeSkeleton.codeAttribute(9, 2, 1, instructions, Seq(), Seq(lineNumberTable))),
      Set(ByteCodeSkeleton.PublicAccess, ByteCodeSkeleton.StaticAccess))
  }
}
