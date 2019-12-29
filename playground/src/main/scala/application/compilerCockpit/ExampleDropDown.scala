package application.compilerCockpit

import java.awt.event.ActionEvent

import core.SourceUtils
import javax.swing.{DefaultComboBoxModel, JComboBox, JLabel, JPanel}
import util.JavaSourceUtils

import scala.reflect.io.Path

class ExampleDropDown(val compilerCockpit: LanguageSandbox) extends JPanel {

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
    val content = SourceUtils.getResourceFileContents("FibonacciInSimplifiedByteCode.txt")
    new Example("Fibonacci Simplified Bytecode", content)
  }

  def getFibonacci = {
    val fibonacciContent = JavaSourceUtils.getJavaTestFileContents("Fibonacci.java", Path(""))
    new Example("Fibonacci", fibonacciContent)
  }

  def getFibonacciByteCode = {
    val fibonacciContent = SourceUtils.getResourceFileContents("FibonacciByteCodePrettyPrinted.txt")
    new Example("Fibonacci Basic Bytecode", fibonacciContent)
  }

  def getForLoop = {
    val fibonacciContent = JavaSourceUtils.getJavaTestFileContents("SimpleForLoop", Path(""))
    new Example("For Loop", fibonacciContent)
  }

  def getWhile = {
    val fibonacciContent = JavaSourceUtils.getJavaTestFileContents("Whilee", Path(""))
    new Example("While", fibonacciContent)
  }

  def getFibonacciExpressionMethod = {
    val fibonacciContent = JavaSourceUtils.getJavaTestFileContents("FibonacciWithExpressionMethod", Path(""))
    new Example("Fibonacci with expression method", fibonacciContent)
  }

  def getRevealSyntaxSugar = {
    new Example("Filled with syntax sugar", JavaSourceUtils.getJavaTestFileContents("RevealSyntaxSugar.java"))
  }

  def getVariableAlreadyDefined = {
    new Example("Variable defined twice", JavaSourceUtils.getJavaTestFileContents("VariableAlreadyDefined", Path("")))
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
    val content = SourceUtils.getResourceFileContents("MethodOverloading.java")
    new Example("MethodOverloading", content)
  }

  def getFibonacciWithLabelledLocations = {
    val content = SourceUtils.getResourceFileContents("FibonacciWithLabelledLocations.txt")
    new Example("Fibonacci with labelled locations", content)
  }

  private def reorderMembers = {
    val content =
      """class Example
        |{
        |    int first;
        |
        |    public static int second;
        |}
      """.stripMargin
    Example("Reorder members", content)
  }

  private def reorderMembersWithComments = {
    val content =
      """class Example
        |{
        |    int first;
        |
        |    /* second is used for foo */
        |    public static /* bar */ int second;
        |}
      """.stripMargin
    Example("Reorder members with comments", content)
  }

  def initialise(): Unit =  {
    try
    {
      val exampleModel = new DefaultComboBoxModel[Example](Array(getFibonacci, getForLoop, getWhile,
        getVariableAlreadyDefined, getFibonacciExpressionMethod, getFibonacciSimplifiedByteCode, getFibonacciByteCode, getRevealSyntaxSugar,
        getComparisonOptimization, getFibonacciWithComments, getMethodOverloading, getFibonacciWithLabelledLocations,
        reorderMembers, reorderMembersWithComments))
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
