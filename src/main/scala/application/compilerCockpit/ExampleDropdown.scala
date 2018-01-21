package application.compilerCockpit

import java.awt.event.ActionEvent
import javax.swing.{DefaultComboBoxModel, JComboBox, JLabel, JPanel}

import util.SourceUtils

import scala.reflect.io.Path

class ExampleDropdown(val compilerCockpit: LanguageSandbox) extends JPanel {

  initialise()

  case class Example(name: String, content: String)
  {
    override def toString = name
  }

  def getComparisonOptimization = {
    Example("ComparisonOptimization",
      """class ComparisonOptimization
        |{
        |    public static void main(java.lang.String[] args)
        |    {
        |        canOptimize();
        |        cannotOptimize();
        |    }
        |
        |    private static void cannotOptimize() {
        |        boolean x = 2 < 3;
        |        if (x)
        |            System.out.print(x);
        |    }
        |
        |    private static void canOptimize() {
        |        if (2 < 3)
        |            System.out.print(5);
        |    }
        |}
      """.stripMargin)
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
    Example("Fibonacci with comments",
    """class Fibonacci
      |{
      |    /* Program starts here */
      |    public static void main(java.lang.String[] args)
      |    {
      |        System.out.print(Fibonacci.fibonacci(5));
      |    }
      |
      |    public static int fibonacci(int index)
      |    {
      |        return index < /* Placed left of 2 */ 2 ? 1 : Fibonacci.fibonacci(index - 1) + Fibonacci.fibonacci(index - 2);
      |    }
      |}""".stripMargin)
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
      case e:Exception => compilerCockpit.setOutputText("Could not load example dropdown, because: " + e.toString) //TODO move to Logger.
    }
  }

}
