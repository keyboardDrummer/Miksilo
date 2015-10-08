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

  def getFibonacci = {
    val fibonacciContent = TestUtils.getJavaTestFile("Fibonacci", Path("")).slurp()
    new Example("Fibonacci", fibonacciContent)
  }

  def getFibonacciByteCode = {
    val fibonacciContent = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()
    new Example("Basic Fibonacci Bytecode", fibonacciContent)
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
    val fibonacciContent = TestUtils.getJavaTestFile("VariableAlreadyDefined", Path("")).slurp()
    new Example("Variable defined twice", fibonacciContent)
  }

  def initialise() {
    val exampleModel = new DefaultComboBoxModel[Example](Array(getFibonacci, getForLoop, getWhile,
      getVariableAlreadyDefined, getFibonacciExpressionMethod, getFibonacciByteCode, getRevealSyntaxSugar))
    add(new JLabel("code examples:"))
    val comboBox: JComboBox[Example] = new JComboBox(exampleModel)

    comboBox.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = compilerCockpit.setInputText(exampleModel.getSelectedItem.asInstanceOf[Example].content)
    })
    add(comboBox)
  }

}
