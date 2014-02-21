package languages.java.testing

import languages.bytecode.{LineNumberRef, ByteCode}
import languages.java.base.{JavaMethodModel, JavaClassModel, JavaTypes}
import org.junit.{Assert, Test}
import languages.java.JavaCompiler
import java.util
import transformation.{ComparisonOptions, MetaObject}
import java.util.Comparator
import scala.collection.mutable

class TestEmptyClassCompilation {
  val classname: String = "EmptyClass"

  def getEmptyClassByteCode() = {
    val constantPool = Seq(ByteCode.methodRef(3, 10),
      ByteCode.classRef(11),
      ByteCode.classRef(12),
      ByteCode.constructorName,
      ByteCode.methodDescriptor(JavaTypes.VoidType, Seq()),
      ByteCode.CodeAttributeId,
      ByteCode.LineNumberTableId,
      ByteCode.SourceFileId,
      "EmptyClass.java",
      ByteCode.nameAndType(4, 5),
      "languages/java/testing/EmptyClass",
      "java/lang/Object"
    )
    val lineNumberTable = ByteCode.lineNumberTable(6, 0, Seq(new LineNumberRef(3,0)))
    val instructions = Seq(ByteCode.addressLoad(0), ByteCode.invokeSpecial(1), ByteCode.voidReturn)
    val codeAttribute = Seq(ByteCode.codeAttribute(5, 0, 1, 1, instructions, Seq(), Seq(lineNumberTable)))
    val defaultConstructor = ByteCode.methodInfo(3,4, codeAttribute)
    ByteCode.clazz(classname, constantPool, Seq(defaultConstructor))
  }

  def getEmptyClass() = {
    JavaClassModel.clazz(classname, Seq[MetaObject]())
  }

  @Test
  def testAll() {
    val expectedByteCode = getEmptyClassByteCode()
    val javaCode = getEmptyClass()
    val compiledCode = JavaCompiler.getCompiler.compile(javaCode)
    val expectedConstantPoolSet = ByteCode.getConstantPool(expectedByteCode)
    val compiledConstantPoolSet = ByteCode.getConstantPool(compiledCode)
    Assert.assertEquals(expectedConstantPoolSet.length, compiledConstantPoolSet.length)
    Assert.assertTrue(expectedConstantPoolSet.forall(expectedItem =>
      compiledConstantPoolSet.exists(compiledItem => MetaObject.deepEquality(compiledItem,expectedItem,
      new ComparisonOptions(false,false,true)))))

    val expectedMethod = ByteCode.getMethods(expectedByteCode)(0)
    val compiledMethod = ByteCode.getMethods(compiledCode)(0)
    Assert.assertTrue(MetaObject.deepEquality(expectedMethod,compiledMethod,
        new ComparisonOptions(false,false,true)))

  }
}
