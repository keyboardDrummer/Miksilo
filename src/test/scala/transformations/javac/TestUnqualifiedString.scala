package transformations.javac

import core.particles.node.Node
import org.scalatest.FunSuite
import transformations.bytecode.types.IntTypeC
import transformations.javac.classes.skeleton.MethodClassKey
import util.TestUtils

class TestUnqualifiedString extends FunSuite {

  test("basic") {
    TestUtils.compareWithJavacAfterRunning("UnqualifiedString")
  }
}
