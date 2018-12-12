package deltas.bytecode.readJar

import java.io

import core.bigrammar.{BiGrammarToParser, TestLanguageGrammarUtils}
import core.language.node.Node
import deltas.PrettyPrint
import org.scalatest.FunSuite
import deltas.bytecode.types.TypeSkeleton
import deltas.bytecode.types.TypeSkeleton.ByteCodeTypeGrammar
import deltas.javac.JavaLanguage
import deltas.javac.types.TypeAbstraction
import util.{SourceUtils, TestLanguageBuilder}

import scala.reflect.io.{File, Path}
import BiGrammarToParser._

class TestClassFileDecompiler extends FunSuite {

  test("TypeVariableSimilarToBooleanSignature") {
    val signature = "<B:Ljava/lang/Object;V:Ljava/lang/Object;>(Ljava/lang/Class<TB;>;Ljava/lang/String;Ljava/lang/String;)Lcom/sun/xml/internal/bind/api/RawAccessor<TB;TV;>;"
    val compiler = TestLanguageBuilder.buildWithParser(ClassFileSignatureDecompiler.getDecompiler)
    val language = compiler.language

    val grammar = language.grammars.find(TypeAbstraction.AbstractMethodTypeGrammar)
    val parser = toStringParser(grammar)
    val parseResult = parser(signature)
    val result = parseResult.get.asInstanceOf[Node]
  }

  test("TypeVariable") {
    val signature = "<NoSuchMemberException:Ljava/lang/ReflectiveOperationException;>(BLjava/lang/invoke/MemberName;Ljava/lang/Class<*>;Ljava/lang/Class<TNoSuchMemberException;>;)Ljava/lang/invoke/MemberName;^Ljava/lang/IllegalAccessException;"
    val compiler = TestLanguageBuilder.buildWithParser(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.language

    val result = toStringParser(state.grammars.find(TypeSkeleton.ByteCodeTypeGrammar))(signature).get.asInstanceOf[Node]
  }

  test("TypeVariable2") {
    val signature = "<NoSuchMemberException:Ljava/lang/ReflectiveOperationException;>(BLjava/lang/invoke/MemberName;Ljava/lang/Class<*>;Ljava/lang/Class<TNoSuchMemberException;>;)Ljava/lang/invoke/MemberName;^Ljava/lang/IllegalAccessException;^TNoSuchMemberException;"
    val compiler = TestLanguageBuilder.buildWithParser(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.language

    val parser = toStringParser(state.grammars.find(TypeSkeleton.ByteCodeTypeGrammar))
    val result = parser(signature).get.asInstanceOf[Node]
  }

  test("TypeVariable3") {
    val signature = "(BLjava/lang/invoke/MemberName;Ljava/lang/Class<*>;Ljava/lang/Class<TNoSuchMemberException;>;)Ljava/lang/invoke/MemberName;^Ljava/lang/IllegalAccessException;^TNoSuchMemberException;"
    val compiler = TestLanguageBuilder.buildWithParser(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.language

    val parser = toStringParser(state.grammars.find(TypeSkeleton.ByteCodeTypeGrammar))
    val result = parser(signature).get.asInstanceOf[Node]
  }

  ignore("decompileRuntimeJar") {
    val currentDir = new File(new io.File("."))
    val testResources = currentDir / Path("resources") / "rtUnzipped"
    val allCassFiles = testResources.toDirectory.deepFiles
    val compiler = TestLanguageBuilder.buildWithParser(/*Seq(new PrettyPrint()) ++*/ ClassFileSignatureDecompiler.getDecompiler)
    var counter = 0
    val start = 17453
    for(file <- allCassFiles) {
      if (counter >= start && file.extension.contains("class")) {
        val inputStream = file.inputStream()
        Console.println(s"starting: ${file.name}")
        compiler.compile(inputStream)
        Console.println(s"progress: $counter / 19.626")
      }
      counter += 1
    }
  }

  test("ObjectClassUnParsedAttributes") {
    val inputStream = SourceUtils.getTestFile("Object.class")
    val compiler = TestLanguageBuilder.build(Seq(DecodeByteCodeParser, PrettyPrint()) ++ ClassFileSignatureDecompiler.byteCodeParticles)
    val state = compiler.compile(inputStream)

    val expected = SourceUtils.getTestFileContents("DecodedObjectClassPrettyPrint.txt")
    assertResult(expected)(state.output)
  }

  test("ObjectClassParsedAttributes") {
    val compiler = TestLanguageBuilder.build(Seq(DecodeByteCodeParser, ParseKnownAttributes) ++ Seq(PrettyPrint()) ++
      ClassFileSignatureDecompiler.onlySignatureAttribute)
    val state = compiler.compile(SourceUtils.getTestFile("Object.class"))

    val expected = SourceUtils.getTestFileContents("DecodedWithAttributesObjectClassPrettyPrint.txt")
    assertResult(expected)(state.output)
  }

  test("ObjectClassSignatureDeCompilation") {
    val compiler = TestLanguageBuilder.build(ClassFileSignatureDecompiler.getDecompiler)
    val compilation = compiler.compile(SourceUtils.getTestFile("Object.class"))
    val output = TestLanguageBuilder.build(Seq(PrettyPrint()) ++ JavaLanguage.javaCompilerDeltas).compileAst(compilation.program).output

    val expected = SourceUtils.getTestFileContents("DecompiledObjectClassFileSignature.txt")
    assertResult(expected)(output)
  }

  test("StringClassSignatureDeCompilation") {
    val compiler = TestLanguageBuilder.build(ClassFileSignatureDecompiler.getDecompiler)
    val compilation = compiler.compile(SourceUtils.getTestFile("String2.class"))
    val output = TestLanguageBuilder.build(Seq(PrettyPrint()) ++ JavaLanguage.javaCompilerDeltas).compileAst(compilation.program).output

    val expected = SourceUtils.getTestFileContents("DecompiledStringClassFileSignature.txt")
    assertResult(expected)(output)
  }

  test("SystemClassSignatureDeCompilation") {
    val compiler = TestLanguageBuilder.build(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.compile(SourceUtils.getTestFile("System.class"))
    val output = TestLanguageBuilder.build(Seq(PrettyPrint()) ++ JavaLanguage.javaCompilerDeltas).compileAst(state.program).output

    val expected = SourceUtils.getTestFileContents("DecompiledSystemClassFileSignature.txt")
    assertResult(expected)(output)
  }

  test("PrintStreamClassSignatureDeCompilation") {
    val compiler = TestLanguageBuilder.build(ClassFileSignatureDecompiler.getDecompiler)
    val state = compiler.compile(SourceUtils.getTestFile("PrintStream.class"))
    val output = TestLanguageBuilder.build(Seq(new PrettyPrint()) ++ JavaLanguage.javaCompilerDeltas).compileAst(state.program).output

    val expected = SourceUtils.getTestFileContents("DecompiledPrintStreamClassFileSignature.txt")
    assertResult(expected)(output)
  }

  test("ParseByteCodeType3") {
    val grammarUtils = TestLanguageGrammarUtils(ClassFileSignatureDecompiler.getDecompiler)
    grammarUtils.compareInputWithPrint("([BII)V", None, ByteCodeTypeGrammar)
  }


  test("ParseByteCodeType2") {
    val grammarUtils = TestLanguageGrammarUtils(ClassFileSignatureDecompiler.getDecompiler)
    grammarUtils.compareInputWithPrint("[B", None, ByteCodeTypeGrammar)
  }

  test("ParseByteCodeType1") {
    val grammarUtils = TestLanguageGrammarUtils(ClassFileSignatureDecompiler.getDecompiler)
    grammarUtils.compareInputWithPrint("B", None, ByteCodeTypeGrammar)
  }

  test("ParseByteCodeType0") {
    val grammarUtils = TestLanguageGrammarUtils(ClassFileSignatureDecompiler.getDecompiler)
    grammarUtils.compareInputWithPrint("I", None, ByteCodeTypeGrammar)
  }
}
