package transformations.javac

import core.particles.node.Node
import org.scalatest.FunSuite
import transformations.bytecode.types.IntTypeC
import transformations.javac.classes.skeleton.MethodClassKey
import util.TestUtils

class TestUnqualifiedString extends FunSuite {

  test("sequenceEquality") {
    val result: Boolean = MethodClassKey("print",Vector[Node](IntTypeC.intType)).equals(MethodClassKey("print",List[Node](IntTypeC.intType)))
    assert(result)
  }

  test("basic") {
    TestUtils.compareWithJavacAfterRunning("UnqualifiedString")
  }
}
