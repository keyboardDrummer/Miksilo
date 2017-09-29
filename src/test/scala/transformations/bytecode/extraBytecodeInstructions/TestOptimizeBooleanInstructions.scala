package transformations.bytecode.extraBytecodeInstructions

import core.particles.node.Node
import org.scalatest.FunSuite
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.extraBooleanInstructions.OptimizeComparisonInstructionsC
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import transformations.javac.JavaCompiler
import util.{CompilerBuilder, TestUtils}

import scala.reflect.io.Path

class TestOptimizeBooleanInstructions extends FunSuite {

  test("ForFibonacci") {
    val withOptimization = TestUtils.parseAndTransform("Fibonacci", Path(""))
    val withoutOptimizationTransformations = JavaCompiler.javaCompilerTransformations.filter(i => i != OptimizeComparisonInstructionsC)
    val withoutOptimization = new TestUtils(CompilerBuilder.build(withoutOptimizationTransformations)).parseAndTransform("Fibonacci", Path(""))

    val unoptimizedInstructions = getFibonacciInstructions(withoutOptimization)
    val optimizedInstructions = getFibonacciInstructions(withOptimization)

    assert(optimizedInstructions.size + 3 < unoptimizedInstructions.size,
      s"optimizedInstructions.size (${optimizedInstructions.size}) + 5 < unoptimizedInstructions.size (${unoptimizedInstructions.size})")
  }

  def getFibonacciInstructions(clazz: Node) = {
    ByteCodeSkeleton.getMethods(clazz)
      .flatMap(methodInfo => ByteCodeMethodInfo.getMethodAttributes(methodInfo))
      .flatMap(annotation => if (annotation.clazz == CodeAttribute.CodeKey) Some(annotation) else None)
      .flatMap(codeAnnotation => CodeAttribute.getCodeInstructions(codeAnnotation))
  }
}

