package transformations.bytecode.readJar

import java.io

import application.compilerCockpit.PrettyPrint
import core.bigrammar.TestGrammarUtils
import core.particles.node.Node
import core.particles.{CompilerFromDeltas, DeltasToParserConverter}
import org.scalatest.FunSuite
import transformations.bytecode.types.TypeSkeleton
import transformations.bytecode.types.TypeSkeleton.ByteCodeTypeGrammar
import transformations.javac.JavaCompiler
import transformations.javac.types.TypeAbstraction
import util.{CompilerBuilder, TestUtils}

import scala.reflect.io.{File, Path}

class TestClassFileDecompiler extends FunSuite {

  test("TypeVariableSimilarToBooleanSignature") {
    val signature = "<B:Ljava/lang/Object;V:Ljava/lang/Object;>(Ljava/lang/Class<TB;>;Ljava/lang/String;Ljava/lang/String;)Lcom/sun/xml/internal/bind/api/RawAccessor<TB;TV;>;"
    val compiler = CompilerBuilder.build(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.buildLanguage

    val manager = new DeltasToParserConverter()
    val result = manager.parse(state.grammarCatalogue.find(TypeAbstraction.AbstractMethodTypeGrammar), signature).asInstanceOf[Node]
  }

  test("TypeVariable") {
    val signature = "<NoSuchMemberException:Ljava/lang/ReflectiveOperationException;>(BLjava/lang/invoke/MemberName;Ljava/lang/Class<*>;Ljava/lang/Class<TNoSuchMemberException;>;)Ljava/lang/invoke/MemberName;^Ljava/lang/IllegalAccessException;"
    val compiler = CompilerBuilder.build(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.buildLanguage

    val manager = new DeltasToParserConverter()
    val result = manager.parse(state.grammarCatalogue.find(TypeSkeleton.ByteCodeTypeGrammar), signature).asInstanceOf[Node]
  }

  test("TypeVariable2") {
    val signature = "<NoSuchMemberException:Ljava/lang/ReflectiveOperationException;>(BLjava/lang/invoke/MemberName;Ljava/lang/Class<*>;Ljava/lang/Class<TNoSuchMemberException;>;)Ljava/lang/invoke/MemberName;^Ljava/lang/IllegalAccessException;^TNoSuchMemberException;"
    val compiler = CompilerBuilder.build(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.buildLanguage

    val manager = new DeltasToParserConverter()
    val result = manager.parse(state.grammarCatalogue.find(TypeSkeleton.ByteCodeTypeGrammar), signature).asInstanceOf[Node]
  }

  test("TypeVariable3") {
    val signature = "(BLjava/lang/invoke/MemberName;Ljava/lang/Class<*>;Ljava/lang/Class<TNoSuchMemberException;>;)Ljava/lang/invoke/MemberName;^Ljava/lang/IllegalAccessException;^TNoSuchMemberException;"
    val compiler = CompilerBuilder.build(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.buildLanguage

    val manager = new DeltasToParserConverter()
    val result = manager.parse(state.grammarCatalogue.find(TypeSkeleton.ByteCodeTypeGrammar), signature).asInstanceOf[Node]
  }

  ignore("decompileRuntimeJar") {
    val currentDir = new File(new io.File("."))
    val testResources = currentDir / Path("resources") / "rtUnzipped"
    val allCassFiles = testResources.toDirectory.deepFiles
    val compiler = CompilerBuilder.build(/*Seq(new PrettyPrint()) ++*/ ClassFileSignatureDecompiler.getDecompiler)
    var counter = 0
    val start = 17453
    for(file <- allCassFiles) {
      if (counter >= start && file.extension.contains("class")) {
        val inputStream = file.inputStream()
        Console.println(s"starting: ${file.name}")
        compiler.parseAndTransform(inputStream)
        Console.println(s"progress: $counter / 19.626")
      }
      counter += 1
    }
  }

  test("ObjectClassUnParsedAttributes") {
    val inputStream = TestUtils.getTestFile("Object.class")
    val compiler = CompilerBuilder.build(Seq(new PrettyPrint()) ++ ClassFileSignatureDecompiler.byteCodeParticles)
    val state = compiler.parseAndTransform(inputStream)

    val expected = TestUtils.getTestFileContents("DecodedObjectClassPrettyPrint.txt")
    assertResult(expected)(state.output)
  }

  test("ObjectClassParsedAttributes") {
    val compiler = CompilerBuilder.build(Seq(ParseKnownAttributes) ++ Seq(new PrettyPrint()) ++
      ClassFileSignatureDecompiler.onlySignatureAttribute)
    val state = compiler.parseAndTransform(TestUtils.getTestFile("Object.class"))

    val expected = TestUtils.getTestFileContents("DecodedWithAttributesObjectClassPrettyPrint.txt")
    assertResult(expected)(state.output)
  }

  test("ObjectClassSignatureDeCompilation") {
    val compiler = CompilerBuilder.build(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.parseAndTransform(TestUtils.getTestFile("Object.class"))
    val output = CompilerBuilder.build(Seq(new PrettyPrint()) ++ JavaCompiler.javaCompilerTransformations).transform(state.program).output

    val expected = TestUtils.getTestFileContents("DecompiledObjectClassFileSignature.txt")
    assertResult(expected)(output)
  }

  test("StringClassSignatureDeCompilation") {
    val compiler = CompilerBuilder.build(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.parseAndTransform(TestUtils.getTestFile("String.class"))
    val output = CompilerBuilder.build(Seq(new PrettyPrint()) ++ JavaCompiler.javaCompilerTransformations).transform(state.program).output

    val expected = TestUtils.getTestFileContents("DecompiledStringClassFileSignature.txt")
    assertResult(expected)(output)
  }

  test("SystemClassSignatureDeCompilation") {
    val compiler = CompilerBuilder.build(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.parseAndTransform(TestUtils.getTestFile("System.class"))
    val output = CompilerBuilder.build(Seq(new PrettyPrint()) ++ JavaCompiler.javaCompilerTransformations).transform(state.program).output

    val expected = TestUtils.getTestFileContents("DecompiledSystemClassFileSignature.txt")
    assertResult(expected)(output)
  }

  test("PrintStreamClassSignatureDeCompilation") {
    val compiler = CompilerBuilder.build(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.parseAndTransform(TestUtils.getTestFile("PrintStream.class"))
    val output = CompilerBuilder.build(Seq(new PrettyPrint()) ++ JavaCompiler.javaCompilerTransformations).transform(state.program).output

    val expected = TestUtils.getTestFileContents("DecompiledPrintStreamClassFileSignature.txt")
    assertResult(expected)(output)
  }

  test("ParseByteCodeType3") {
    val grammarUtils = TestGrammarUtils(ClassFileSignatureDecompiler.getDecompiler)
    grammarUtils.compareInputWithPrint("([BII)V", None, ByteCodeTypeGrammar)
  }


  test("ParseByteCodeType2") {
    val grammarUtils = TestGrammarUtils(ClassFileSignatureDecompiler.getDecompiler)
    grammarUtils.compareInputWithPrint("[B", None, ByteCodeTypeGrammar)
  }

  test("ParseByteCodeType1") {
    val grammarUtils = TestGrammarUtils(ClassFileSignatureDecompiler.getDecompiler)
    grammarUtils.compareInputWithPrint("B", None, ByteCodeTypeGrammar)
  }

  test("ParseByteCodeType0") {
    val grammarUtils = TestGrammarUtils(ClassFileSignatureDecompiler.getDecompiler)
    grammarUtils.compareInputWithPrint("I", None, ByteCodeTypeGrammar)
  }
}
