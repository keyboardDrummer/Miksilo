package deltas.javac.expressions

import org.junit.Test
import util.LanguageTest

class TestBooleans {


  def test() {
    LanguageTest.compareWithJavacAfterRunning("Booleans")
  }
}
