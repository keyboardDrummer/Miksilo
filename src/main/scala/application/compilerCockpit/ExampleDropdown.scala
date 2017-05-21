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
    val content = TestUtils.getTestFileContents("ComparisonOptimization.java")
    new Example("ComparisonOptimization", content)
  }

  def getFibonacciSimplifiedByteCode = {
    val content = TestUtils.getTestFileContents("FibonacciInSimplifiedByteCode.txt")
    new Example("Fibonacci Simplified Bytecode", content)
  }

  def getFibonacci = {
    val fibonacciContent = TestUtils.getJavaTestFileContents("Fibonacci.java", Path(""))
    new Example("Fibonacci", fibonacciContent)
  }

  def getFibonacciByteCode = {
    val fibonacciContent = TestUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    new Example("Fibonacci Basic Bytecode", fibonacciContent)
  }

  def getForLoop = {
    val fibonacciContent = TestUtils.getJavaTestFileContents("SimpleForLoop", Path(""))
    new Example("For Loop", fibonacciContent)
  }

  def getWhile = {
    val fibonacciContent = TestUtils.getJavaTestFileContents("Whilee", Path(""))
    new Example("While", fibonacciContent)
  }

  def getFibonacciExpressionMethod = {
    val fibonacciContent = TestUtils.getJavaTestFileContents("FibonacciWithExpressionMethod", Path(""))
    new Example("Fibonacci with expression method", fibonacciContent)
  }

  def getRevealSyntaxSugar = {
    new Example("Filled with syntax sugar", TestUtils.getJavaTestFileContents("RevealSyntaxSugar.java"))
  }

  def getVariableAlreadyDefined = {
    new Example("Variable defined twice", TestUtils.getJavaTestFileContents("VariableAlreadyDefined", Path("")))
  }

  def getFibonacciWithComments = {
    new Example("Fibonacci with comments", TestUtils.getJavaTestFileContents("FibonacciWithComments.java"))
  }

  def getMethodOverloading = {
    val content = TestUtils.getTestFileContents("MethodOverloading.java")
    new Example("MethodOverloading", content)
  }

  def getFibonacciWithLabelledLocations = {
    val content = TestUtils.getTestFileContents("FibonacciWithLabelledLocations.txt")
    new Example("Fibonacci with labelled locations", content)
  }

  def initialise() {
    try
    {
      val exampleModel = new DefaultComboBoxModel[Example](Array(getFibonacci, getForLoop, getWhile,
        getVariableAlreadyDefined, getFibonacciExpressionMethod, getFibonacciSimplifiedByteCode, getFibonacciByteCode, getRevealSyntaxSugar,
        getComparisonOptimization, getFibonacciWithComments, getMethodOverloading, getFibonacciWithLabelledLocations))
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
