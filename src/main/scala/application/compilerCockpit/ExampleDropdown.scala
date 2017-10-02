package application.compilerCockpit

import java.awt.event.ActionEvent
import javax.swing.{DefaultComboBoxModel, JComboBox, JLabel, JPanel}

import util.SourceUtils

import scala.reflect.io.Path

class ExampleDropdown(val compilerCockpit: CompilerCockpit) extends JPanel {

  initialise()

  case class Example(name: String, content: String)
  {
    override def toString = name
  }

  def getComparisonOptimization = {
    val content = SourceUtils.getTestFileContents("ComparisonOptimization.java")
    new Example("ComparisonOptimization", content)
  }

  def getFibonacciSimplifiedByteCode = {
    val content = SourceUtils.getTestFileContents("FibonacciInSimplifiedByteCode.txt")
    new Example("Fibonacci Simplified Bytecode", content)
  }

  def getFibonacci = {
    val fibonacciContent = SourceUtils.getJavaTestFileContents("Fibonacci.java", Path(""))
    new Example("Fibonacci", fibonacciContent)
  }

  def getFibonacciByteCode = {
    val fibonacciContent = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    new Example("Fibonacci Basic Bytecode", fibonacciContent)
  }

  def getForLoop = {
    val fibonacciContent = SourceUtils.getJavaTestFileContents("SimpleForLoop", Path(""))
    new Example("For Loop", fibonacciContent)
  }

  def getWhile = {
    val fibonacciContent = SourceUtils.getJavaTestFileContents("Whilee", Path(""))
    new Example("While", fibonacciContent)
  }

  def getFibonacciExpressionMethod = {
    val fibonacciContent = SourceUtils.getJavaTestFileContents("FibonacciWithExpressionMethod", Path(""))
    new Example("Fibonacci with expression method", fibonacciContent)
  }

  def getRevealSyntaxSugar = {
    new Example("Filled with syntax sugar", SourceUtils.getJavaTestFileContents("RevealSyntaxSugar.java"))
  }

  def getVariableAlreadyDefined = {
    new Example("Variable defined twice", SourceUtils.getJavaTestFileContents("VariableAlreadyDefined", Path("")))
  }

  def getFibonacciWithComments = {
    new Example("Fibonacci with comments", SourceUtils.getJavaTestFileContents("FibonacciWithComments.java"))
  }

  def getMethodOverloading = {
    val content = SourceUtils.getTestFileContents("MethodOverloading.java")
    new Example("MethodOverloading", content)
  }

  def getFibonacciWithLabelledLocations = {
    val content = SourceUtils.getTestFileContents("FibonacciWithLabelledLocations.txt")
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

      comboBox.addActionListener((e: ActionEvent) => compilerCockpit.setInputText(exampleModel.getSelectedItem.asInstanceOf[Example].content))
      add(comboBox)
    }
    catch
    {
      case e:RuntimeException => compilerCockpit.setOutputText("Could not load example dropdown, because: " + e.toString) //TODO move to Logger.
    }
  }

}
