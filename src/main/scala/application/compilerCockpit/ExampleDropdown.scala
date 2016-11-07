package application.compilerCockpit

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{DefaultComboBoxModel, JComboBox, JLabel, JPanel}

import util.TestUtils

import scala.reflect.io.Path

class ExampleDropdown(val compilerCockpit: CompilerCockpit) extends JPanel {

  initialise()

  case class Example(name: String, content: String)
  {
    override def toString = name
  }

  def getComparisonOptimization = {
    val content = TestUtils.getTestFile("ComparisonOptimization.java").slurp()
    new Example("ComparisonOptimization", content)
  }

  def getFibonacciSimplifiedByteCode = {
    val content = TestUtils.getTestFile("FibonacciInSimplifiedByteCode.txt").slurp()
    new Example("Fibonacci Simplified Bytecode", content)
  }

  def getFibonacci = {
    val fibonacciContent = TestUtils.getJavaTestFile("Fibonacci.java", Path("")).slurp()
    new Example("Fibonacci", fibonacciContent)
  }

  def getFibonacciByteCode = {
    val fibonacciContent = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()
    new Example("Fibonacci Basic Bytecode", fibonacciContent)
  }

  def getForLoop = {
    val fibonacciContent = TestUtils.getJavaTestFile("SimpleForLoop", Path("")).slurp()
    new Example("For Loop", fibonacciContent)
  }

  def getWhile = {
    val fibonacciContent = TestUtils.getJavaTestFile("Whilee", Path("")).slurp()
    new Example("While", fibonacciContent)
  }

  def getFibonacciExpressionMethod = {
    val fibonacciContent = TestUtils.getJavaTestFile("FibonacciWithExpressionMethod", Path("")).slurp()
    new Example("Fibonacci with expression method", fibonacciContent)
  }

  def getRevealSyntaxSugar = {
    new Example("Filled with syntax sugar", TestUtils.getJavaTestFile("RevealSyntaxSugar.java").slurp())
  }

  def getVariableAlreadyDefined = {
    new Example("Variable defined twice", TestUtils.getJavaTestFile("VariableAlreadyDefined", Path("")).slurp())
  }

  def getFibonacciWithComments = {
    new Example("Fibonacci with comments", TestUtils.getJavaTestFile("FibonacciWithComments.java").slurp())
  }

  def getMethodOverloading = {
    val content = TestUtils.getTestFile("MethodOverloading.java").slurp()
    new Example("MethodOverloading", content)
  }

  def initialise() {
    try
    {
      val exampleModel = new DefaultComboBoxModel[Example](Array(getFibonacci, getForLoop, getWhile,
        getVariableAlreadyDefined, getFibonacciExpressionMethod, getFibonacciSimplifiedByteCode, getFibonacciByteCode, getRevealSyntaxSugar,
        getComparisonOptimization, getFibonacciWithComments, getMethodOverloading))
      add(new JLabel("code examples:"))
      val comboBox: JComboBox[Example] = new JComboBox(exampleModel)

      comboBox.addActionListener(new ActionListener {
        override def actionPerformed(e: ActionEvent): Unit = compilerCockpit.setInputText(exampleModel.getSelectedItem.asInstanceOf[Example].content)
      })
      add(comboBox)
    }
    catch
    {
      case e:RuntimeException => compilerCockpit.setOutputText("Could not load example dropdown, because: " + e.toString) //TODO move to Logger.
    }
  }

}
