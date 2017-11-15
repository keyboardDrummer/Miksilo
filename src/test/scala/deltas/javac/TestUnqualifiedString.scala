package deltas.javac

import core.particles.node.Node
import org.scalatest.FunSuite
import deltas.bytecode.types.IntTypeC
import deltas.javac.classes.skeleton.MethodClassKey
import util.TestUtils

class TestUnqualifiedString extends FunSuite {

  test("basic") {
    TestUtils.compareWithJavacAfterRunning("UnqualifiedString")
  }
}
