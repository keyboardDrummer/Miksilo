package transformations.javac.methods

import org.junit.{Assert, Ignore, Test}

class TestMethodC {

  @Ignore
  @Test
  def testMethodMustReturn() = {
    Assert.fail("create an example where the method doesn't return void and doesn't end in a return statement.")
  }
}
