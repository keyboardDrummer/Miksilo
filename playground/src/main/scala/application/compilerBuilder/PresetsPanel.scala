package application.compilerBuilder

import java.awt.event._
import java.awt.{GridBagConstraints, GridBagLayout}

import javax.swing._
import javax.swing.event.ListSelectionEvent
import javax.swing.text.AbstractDocument
import application.StyleSheet
import application.compilerCockpit.MarkOutputGrammar
import core.SolveConstraintsDelta
import core.deltas.{Delta, ParseUsingTextualGrammar}
import deltas.bytecode.ByteCodeLanguage
import deltas.bytecode.simpleBytecode.LabelledLocations
import deltas.javaPlus.ExpressionMethodDelta
import deltas.javac.JavaToByteCodeLanguage.getJava
import deltas.javac._
import deltas.javac.classes.FieldDeclarationWithInitializer
import deltas.javac.constructor.{ConstructorDelta, DefaultConstructorDelta, ImplicitSuperConstructorCall}
import deltas.javac.methods.{BlockLanguageDelta, ImplicitReturnAtEndOfMethod}
import deltas.javac.statements.ForLoopContinueDelta
import deltas.json.JsonLanguage
import deltas.statement.assignment.AddAssignmentDelta
import deltas.statement.{ForLoopDelta, LocalDeclarationWithInitializerDelta}

object PresetsPanel
{
  def getSimplifiedByteCodePreset: Preset = {
    val deltas = Delta.spliceAndFilterTop(ExtendedByteCode.simpleByteCodeDeltas, ByteCodeLanguage.byteCodeDeltas, Seq(MarkOutputGrammar))
    Preset("Simplified bytecode", Seq(ParseUsingTextualGrammar()) ++ deltas, "Simplified JVM bytecode.")
  }

  def getJavaToSimplifiedByteCodePreset = {
    Preset("Java to simplified bytecode",
      JavaToByteCodeLanguage.spliceBeforeTransformations(ExtendedByteCode.simpleByteCodeDeltas, Seq(MarkOutputGrammar)),
      "Compiles Java into simplified bytecode")
  }

  def getJavaToExtendedByteCodePreset = {
    Preset("Java to extended bytecode",
      JavaToByteCodeLanguage.spliceBeforeTransformations(ExtendedByteCode.allByteCodeDeltas, Seq(MarkOutputGrammar)),
      "Compiles Java into extended bytecode")
  }

  def getJavaCompilerPreset: Preset = {
    Preset("JavaSubset", getJavaCompilerParticles, "Compiles a subset of Java.")
  }

  def getJavaCompilerParticles: Seq[Delta] = {
    JavaToByteCodeLanguage.spliceBeforeTransformations(ByteCodeLanguage.byteCodeDeltas, Seq(MarkOutputGrammar))
  }

  def getPrettyPrintPreset = {
    Preset("Pretty Print Java", Seq(ParseUsingTextualGrammar(), MarkOutputGrammar) ++ JavaToByteCodeLanguage.javaCompilerDeltas,
      "Performs no transformations. Just parses and prints the Java.")
  }

  def getFibonacciExpressionMethodPreset = {
    Preset("Java with expression method", spliceAfterTransformations(Seq(ParseUsingTextualGrammar()), Seq(ExpressionMethodDelta)),
      "Allows you to use an expression as the body of a method.")
  }

  def getBlockCompilerPreset = {
    Preset("Java statement block", spliceAfterTransformations(Seq(ParseUsingTextualGrammar()), Seq(BlockLanguageDelta)),
      "The program consists only of a single statement block.")
  }

  def getByteCodePreset = {
    Preset("Basic bytecode", Seq(ParseUsingTextualGrammar()) ++ ByteCodeLanguage.byteCodeDeltas, "Regular JVM bytecode.")
  }

  def getAddImplicitsPreset: Preset = {
    val implicits = Seq[Delta](ParseUsingTextualGrammar(), ImplicitJavaLangImport, DefaultConstructorDelta, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, SolveConstraintsDelta, ImplicitThisForPrivateMemberSelectionDelta)

    Preset("Reveal Java Implicits", spliceAfterTransformations(implicits, Seq(MarkOutputGrammar)))
  }

  def getRevealSyntaxSugar: Preset = {
    val implicits = Seq[Delta](ParseUsingTextualGrammar(), DefaultConstructorDelta, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, FieldDeclarationWithInitializer,
      ConstructorDelta, ImplicitReturnAtEndOfMethod, AddAssignmentDelta, ForLoopContinueDelta, ForLoopDelta, LocalDeclarationWithInitializerDelta,
      ImplicitJavaLangImport, SolveConstraintsDelta, ImplicitThisForPrivateMemberSelectionDelta)

    Preset("Reveal Syntax Sugar", spliceAfterTransformations(implicits, Seq(MarkOutputGrammar)),
      "Performs all compiler phases that still maintain a valid Java program.")
  }

  def getLabelledLocations = {
    Preset("Labelled JVM locations", Seq[Delta](ParseUsingTextualGrammar(), LabelledLocations, MarkOutputGrammar) ++ ByteCodeLanguage.byteCodeDeltas,
      "Replaces integer offsets by labels to indicate positions in instruction lists.")
  }

  def getJsonFormatter = {
    Preset("Json",
      JsonLanguage.language.topToBottom,
      "JSON")
  }

  def createModel: DefaultListModel[Preset] = {
    val model = new DefaultListModel[Preset]()
    model.addElement(PresetsPanel.getJavaCompilerPreset)
    //model.addElement(getAddImplicitsPreset)
    model.addElement(PresetsPanel.getPrettyPrintPreset)
    model.addElement(getFibonacciExpressionMethodPreset)
    model.addElement(getBlockCompilerPreset)
    model.addElement(getRevealSyntaxSugar)
    model.addElement(PresetsPanel.getJavaToExtendedByteCodePreset)
    model.addElement(PresetsPanel.getJavaToSimplifiedByteCodePreset)
    model.addElement(PresetsPanel.getSimplifiedByteCodePreset)
    model.addElement(getByteCodePreset)
    model.addElement(getLabelledLocations)
    model.addElement(getJsonFormatter)
    model
  }

  def spliceAfterTransformations(top: Seq[Delta], splice: Seq[Delta]): Seq[Delta] =
    Delta.spliceAndFilterBottom(top, getJava.topToBottom, splice)
}

class PresetsPanel(compilerName: AbstractDocument, selectedParticles: DeltaInstanceList)
  extends JPanel(new GridBagLayout()) {

  initialise()

  def initialise(): Unit =  {
    val listConstraints: GridBagConstraints = new GridBagConstraints()
    listConstraints.fill = GridBagConstraints.BOTH
    listConstraints.weightx = 1
    listConstraints.weighty = 1
    listConstraints.gridx = 0
    listConstraints.insets = StyleSheet.defaultInsets

    val model: DefaultListModel[Preset] = PresetsPanel.createModel

    StyleSheet.setTitleBorder(this, "Presets")
    val presetsList = new JList(model) {

      override def getToolTipText(event: MouseEvent): String = {
        val index = this.locationToIndex(event.getPoint)
        val model = this.getModel
        val text: String = model.getElementAt(index).description
        if (text.isEmpty)
          return null
        text
      }
    }

    presetsList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    presetsList.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = {
        if(e.getClickCount == 2) {
          val index = presetsList.locationToIndex(e.getPoint)
          val item = model.getElementAt(index)
          applyPreset(item)
        }
      }
    })

    add(StyleSheet.getAnyListVisuals(presetsList), listConstraints)

    val buttonConstraints: GridBagConstraints = new GridBagConstraints()
    buttonConstraints.fill = GridBagConstraints.HORIZONTAL
    buttonConstraints.weightx = 1
    buttonConstraints.gridx = 0
    buttonConstraints.insets = StyleSheet.defaultInsets
    val applyButton: JButton = getApplyButton(presetsList)
    add(applyButton, buttonConstraints)
  }

  def getApplyButton(presetsList: JList[Preset]): JButton = {
    val applyButton = new JButton()
    applyButton.setText("Apply")
    presetsList.addListSelectionListener((e: ListSelectionEvent) =>
      applyButton.setEnabled(!presetsList.isSelectionEmpty))
    applyButton.addActionListener((e: ActionEvent) => {
      val preset: Preset = presetsList.getSelectedValue
      applyPreset(preset)
    })
    applyButton.setFont(StyleSheet.defaultFont)
    applyButton
  }

  def applyPreset(preset: Preset): Unit = {
    selectedParticles.clear()
    compilerName.replace(0, compilerName.getLength, preset.name, null)
    for (particle <- preset.deltas)
      selectedParticles.addElement(new DeltaInstance(particle))
  }
}

case class Preset(name: String, deltas: Seq[Delta], description: String = "") {
  override def toString: String = name
}
