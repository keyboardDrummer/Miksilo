package deltas.javac

import core.language.node.Node
import org.scalatest.FunSuite
import deltas.bytecode.types.IntTypeDelta
import deltas.javac.classes.skeleton.MethodClassKey
import util.TestUtils

class TestUnqualifiedString extends FunSuite {

  test("basic") {
    TestUtils.compareWithJavacAfterRunning("UnqualifiedString")
  }
}
