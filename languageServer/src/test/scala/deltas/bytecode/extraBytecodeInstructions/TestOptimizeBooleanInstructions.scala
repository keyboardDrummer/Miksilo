package deltas.bytecode.extraBytecodeInstructions

import core.language.node.Node
import org.scalatest.FunSuite
import deltas.bytecode.ByteCodeSkeleton.ClassFile
import deltas.bytecode.extraBooleanInstructions.OptimizeComparisonInstructionsDelta
import deltas.javac.JavaLanguage
import util.TestLanguageBuilder
import util.TestUtils

import scala.reflect.io.Path

class TestOptimizeBooleanInstructions extends FunSuite {

  test("ForFibonacci") {
    val withOptimization = TestUtils.parseAndTransform("Fibonacci", Path(""))
    val withoutOptimizationTransformations = JavaLanguage.javaCompilerDeltas.filter(i => i != OptimizeComparisonInstructionsDelta)
    val withoutOptimization = new TestUtils(TestLanguageBuilder.build(withoutOptimizationTransformations)).parseAndTransform("Fibonacci", Path(""))

    val unoptimizedInstructions = getFibonacciInstructions(withoutOptimization)
    val optimizedInstructions = getFibonacciInstructions(withOptimization)

    assert(optimizedInstructions.size + 3 < unoptimizedInstructions.size,
      s"optimizedInstructions.size (${optimizedInstructions.size}) + 5 < unoptimizedInstructions.size (${unoptimizedInstructions.size})")
  }

  def getFibonacciInstructions(classFile: ClassFile[Node]) = {
    classFile.methods.flatMap(methodInfo => methodInfo.codeAttribute.instructions)
  }
}

