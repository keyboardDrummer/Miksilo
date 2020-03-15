package miksilo.modularLanguages.deltas.bytecode.extraBytecodeInstructions

import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.ByteCodeSkeleton.ClassFile
import miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions.OptimizeComparisonInstructionsDelta
import miksilo.modularLanguages.deltas.javac.JavaToByteCodeLanguage
import miksilo.modularLanguages.util.TestLanguageBuilder
import miksilo.modularLanguagesutil.{JavaLanguageTest, LanguageTest}

import scala.reflect.io.Path

class TestOptimizeBooleanInstructions extends JavaLanguageTest {

  test("ForFibonacci") {
    val withOptimization = parseAndTransform("Fibonacci", Path(""))
    val withoutOptimizationTransformations = JavaToByteCodeLanguage.javaCompilerDeltas.filter(i => i != OptimizeComparisonInstructionsDelta)
    val withoutOptimization = new LanguageTest(TestLanguageBuilder.buildWithParser(withoutOptimizationTransformations)).parseAndTransform("Fibonacci", Path(""))

    val unoptimizedInstructions = getFibonacciInstructions(withoutOptimization)
    val optimizedInstructions = getFibonacciInstructions(withOptimization)

    assert(optimizedInstructions.size + 3 < unoptimizedInstructions.size,
      s"optimizedInstructions.size (${optimizedInstructions.size}) + 5 < unoptimizedInstructions.size (${unoptimizedInstructions.size})")
  }

  def getFibonacciInstructions(classFile: ClassFile[Node]) = {
    classFile.methods.flatMap(methodInfo => methodInfo.codeAttribute.instructions)
  }
}

