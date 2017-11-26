package application.compilerBuilder

import java.awt.event._
import java.awt.{GridBagConstraints, GridBagLayout}
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import javax.swing.text.AbstractDocument

import application.StyleSheet
import application.compilerCockpit.MarkOutputGrammar
import core.deltas.{CompilerFromDeltas, Delta}
import deltas.bytecode.additions.LabelledLocations
import deltas.javaPlus.ExpressionMethodDelta
import deltas.javac._
import deltas.javac.classes.FieldDeclarationWithInitializer
import deltas.javac.constructor.{ConstructorDelta, DefaultConstructorDelta, ImplicitSuperConstructorCall}
import deltas.javac.methods.assignment.IncrementAssignmentC
import deltas.javac.methods.{BlockCompilerDelta, ImplicitReturnAtEndOfMethod}
import deltas.javac.statements.{ForLoopC, ForLoopContinueC}
import deltas.javac.statements.locals.LocalDeclarationWithInitializerC

object PresetsPanel
{
  def getSimplifiedByteCodePreset = {
    new Preset("Simplified bytecode", new CompilerFromDeltas(JavaCompilerDeltas.simpleByteCodeTransformations).
      spliceBeforeTransformations(JavaCompilerDeltas.byteCodeTransformations, Seq(MarkOutputGrammar)),
      "Simplified JVM bytecode.")
  }

  def getJavaToSimplifiedByteCodePreset = {
    new Preset("Java to simplified bytecode",
      JavaCompilerDeltas.spliceBeforeTransformations(JavaCompilerDeltas.simpleByteCodeTransformations, Seq(MarkOutputGrammar)),
      "Compiles Java into simplified bytecode")
  }

  def getJavaToExtendedByteCodePreset = {
    new Preset("Java to extended bytecode",
      JavaCompilerDeltas.spliceBeforeTransformations(JavaCompilerDeltas.allByteCodeTransformations, Seq(MarkOutputGrammar)),
      "Compiles Java into extended bytecode")
  }

  def getJavaCompilerPreset: Preset = {
    new Preset("JavaSubset", getJavaCompilerParticles, "Compiles a subset of Java.")
  }

  def getJavaCompilerParticles: Seq[Delta] = {
    JavaCompilerDeltas.spliceBeforeTransformations(JavaCompilerDeltas.byteCodeTransformations, Seq(MarkOutputGrammar))
  }

  def getPrettyPrintPreset = {
    new Preset("Pretty Print Java", Seq(MarkOutputGrammar) ++ JavaCompilerDeltas.javaCompilerDeltas,
      "Performs no transformations. Just parses and prints the Java.")
  }

  def getFibonacciExpressionMethodPreset = {
    new Preset("Java with expression method", Seq(ExpressionMethodDelta) ++ getJavaCompilerParticles,
      "Allows you to use an expression as the body of a method.")
  }

  def getBlockCompilerPreset = {
    new Preset("Java statement block", Seq(BlockCompilerDelta) ++ getJavaCompilerParticles,
      "The program consists only of a single statement block.")
  }

  def getByteCodePreset = {
    new Preset("Basic bytecode", JavaCompilerDeltas.byteCodeTransformations,
      "Regular JVM bytecode.")
  }

  def getAddImplicitsPreset: Preset = {
    val implicits = Seq[Delta](ImplicitJavaLangImport, DefaultConstructorDelta, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitThisForPrivateMemberSelection, ImplicitReturnAtEndOfMethod)

    new Preset("Reveal Java Implicits", JavaCompilerDeltas.spliceAfterTransformations(implicits, Seq(MarkOutputGrammar)))
  }

  def getRevealSyntaxSugar: Preset = {
    val implicits = Seq[Delta](DefaultConstructorDelta, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, FieldDeclarationWithInitializer,
      ConstructorDelta, ImplicitReturnAtEndOfMethod, IncrementAssignmentC, ForLoopContinueC, ForLoopC, LocalDeclarationWithInitializerC,
      ImplicitThisForPrivateMemberSelection, ImplicitJavaLangImport)

    new Preset("Reveal Syntax Sugar", JavaCompilerDeltas.spliceAfterTransformations(implicits, Seq(MarkOutputGrammar)),
      "Performs all compiler phases that still maintain a valid Java program.")
  }

  def getLabelledLocations = {
    new Preset("Labelled JVM locations", Seq[Delta](LabelledLocations, MarkOutputGrammar) ++ JavaCompilerDeltas.byteCodeTransformations,
      "Replaces integer offsets by labels to indicate positions in instruction lists.")
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
    model
  }
}

class PresetsPanel(compilerName: AbstractDocument, selectedParticles: DeltaInstanceList) extends JPanel(new GridBagLayout()) {

  initialise()

  def initialise() {
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
    presetsList.addListSelectionListener(new ListSelectionListener {
      override def valueChanged(e: ListSelectionEvent): Unit =
        applyButton.setEnabled(!presetsList.isSelectionEmpty)
    })
    applyButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val preset: Preset = presetsList.getSelectedValue
        applyPreset(preset)
      }
    })
    applyButton
  }

  def applyPreset(preset: Preset) {
    selectedParticles.clear()
    compilerName.replace(0, compilerName.getLength, preset.name, null)
    for (particle <- preset.deltas)
      selectedParticles.addElement(new DeltaInstance(particle))
  }
}

case class Preset(name: String, deltas: Seq[Delta], description: String = "") {
  override def toString = name
}
