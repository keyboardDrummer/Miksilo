package application.compilerBuilder

import java.awt.event._
import java.awt.{GridBagConstraints, GridBagLayout}
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import javax.swing.text.AbstractDocument

import application.StyleSheet
import application.compilerCockpit.MarkOutputGrammar
import core.particles.{CompilerFromDeltas, Delta}
import transformations.bytecode.additions.LabelledLocations
import transformations.javaPlus.ExpressionMethodC
import transformations.javac._
import transformations.javac.classes.FieldDeclarationWithInitializer
import transformations.javac.constructor.{ConstructorC, DefaultConstructorC, ImplicitSuperConstructorCall}
import transformations.javac.methods.assignment.IncrementAssignmentC
import transformations.javac.methods.{BlockCompilerC, ImplicitReturnAtEndOfMethod}
import transformations.javac.statements.{ForLoopC, ForLoopContinueC}
import transformations.javac.statements.locals.LocalDeclarationWithInitializerC

object PresetsPanel
{
  def getSimplifiedByteCodePreset = {
    new Preset("Simplified bytecode", new CompilerFromDeltas(JavaCompiler.simpleByteCodeTransformations).
      spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(MarkOutputGrammar)),
      "Simplified JVM bytecode.")
  }

  def getJavaToSimplifiedByteCodePreset = {
    new Preset("Java to simplified bytecode",
      JavaCompiler.spliceBeforeTransformations(JavaCompiler.simpleByteCodeTransformations, Seq(MarkOutputGrammar)),
      "Compiles Java into simplified bytecode")
  }

  def getJavaToExtendedByteCodePreset = {
    new Preset("Java to extended bytecode",
      JavaCompiler.spliceBeforeTransformations(JavaCompiler.allByteCodeTransformations, Seq(MarkOutputGrammar)),
      "Compiles Java into extended bytecode")
  }

  def getJavaCompilerPreset: Preset = {
    new Preset("JavaSubset", getJavaCompilerParticles, "Compiles a subset of Java.")
  }

  def getJavaCompilerParticles: Seq[Delta] = {
    JavaCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(MarkOutputGrammar))
  }

  def getPrettyPrintPreset = {
    new Preset("Pretty Print Java", Seq(MarkOutputGrammar) ++ JavaCompiler.javaCompilerTransformations,
      "Performs no transformations. Just parses and prints the Java.")
  }

  def getFibonacciExpressionMethodPreset = {
    new Preset("Java with expression method", Seq(ExpressionMethodC) ++ getJavaCompilerParticles,
      "Allows you to use an expression as the body of a method.")
  }

  def getBlockCompilerPreset = {
    new Preset("Java statement block", Seq(BlockCompilerC) ++ getJavaCompilerParticles,
      "The program consists only of a single statement block.")
  }

  def getByteCodePreset = {
    new Preset("Basic bytecode", JavaCompiler.byteCodeTransformations,
      "Regular JVM bytecode.")
  }

  def getAddImplicitsPreset: Preset = {
    val implicits = Seq[Delta](ImplicitJavaLangImport, DefaultConstructorC, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitThisForPrivateMemberSelection, ImplicitReturnAtEndOfMethod)

    new Preset("Reveal Java Implicits", JavaCompiler.spliceAfterTransformations(implicits, Seq(MarkOutputGrammar)))
  }

  def getRevealSyntaxSugar: Preset = {
    val implicits = Seq[Delta](DefaultConstructorC, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, FieldDeclarationWithInitializer,
      ConstructorC, ImplicitReturnAtEndOfMethod, IncrementAssignmentC, ForLoopContinueC, ForLoopC, LocalDeclarationWithInitializerC,
      ImplicitThisForPrivateMemberSelection, ImplicitJavaLangImport)

    new Preset("Reveal Syntax Sugar", JavaCompiler.spliceAfterTransformations(implicits, Seq(MarkOutputGrammar)),
      "Performs all compiler phases that still maintain a valid Java program.")
  }

  def getLabelledLocations = {
    new Preset("Labelled JVM locations", Seq[Delta](LabelledLocations, MarkOutputGrammar) ++ JavaCompiler.byteCodeTransformations,
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

class PresetsPanel(compilerName: AbstractDocument, selectedParticles: ParticleInstanceList) extends JPanel(new GridBagLayout()) {

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
    for (particle <- preset.particles)
      selectedParticles.addElement(new ParticleInstance(particle))
  }
}

case class Preset(name: String, particles: Seq[Delta], description: String = "") {
  override def toString = name
}
