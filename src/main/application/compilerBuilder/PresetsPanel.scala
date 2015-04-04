package application.compilerBuilder

import java.awt.event._
import java.awt.{GridBagConstraints, GridBagLayout}
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import application.StyleSheet
import application.compilerCockpit.MarkOutputGrammar
import core.particles.Particle
import transformations.javaPlus.ExpressionMethodC
import transformations.javac._
import transformations.javac.classes.FieldDeclarationWithInitializer
import transformations.javac.constructor.{ConstructorC, DefaultConstructorC, ImplicitSuperConstructorCall}
import transformations.javac.methods.assignment.IncrementAssignmentC
import transformations.javac.methods.{BlockCompilerC, ImplicitReturnAtEndOfMethod}
import transformations.javac.statements.ForLoopC
import transformations.javac.statements.locals.LocalDeclarationWithInitializerC

class PresetsPanel(compilerParticles: DefaultListModel[Particle]) extends JPanel(new GridBagLayout()) {

  initialise()

  def initialise() {
    val listConstraints: GridBagConstraints = new GridBagConstraints()
    listConstraints.fill = GridBagConstraints.BOTH
    listConstraints.weightx = 1
    listConstraints.weighty = 1
    listConstraints.gridx = 0
    listConstraints.insets = StyleSheet.defaultInsets

    val model: DefaultListModel[Preset] = createModel

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

  def createModel: DefaultListModel[Preset] = {
    val model = new DefaultListModel[Preset]()
    model.addElement(getJavaCompilerPreset)
    model.addElement(getAddImplicitsPreset)
    model.addElement(getPrettyPrintPreset)
    model.addElement(getFibonacciExpressionMethodPreset)
    model.addElement(getBlockCompilerPreset)
    model.addElement(getRevealSyntaxSugar)
    model
  }

  def getJavaCompilerPreset: Preset = {
    new Preset("Java", getJavaCompiler)
  }

  def getJavaCompiler: Seq[Particle] = {
    JavaCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(MarkOutputGrammar))
  }

  def getPrettyPrintPreset = {
    new Preset("Pretty Print Java", Seq(MarkOutputGrammar) ++ JavaCompiler.javaCompilerTransformations)
  }

  def getFibonacciExpressionMethodPreset = {
    new Preset("Java with expression method", Seq(ExpressionMethodC) ++ getJavaCompiler)
  }

  def getBlockCompilerPreset = {
    new Preset("Java block", Seq(BlockCompilerC) ++ JavaCompiler.javaCompilerTransformations)
  }

  def getAddImplicitsPreset: Preset = {
    val implicits = Seq[Particle](ImplicitJavaLangImport, DefaultConstructorC, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitThisForPrivateMemberSelection, ImplicitReturnAtEndOfMethod)

    new Preset("Reveal Java Implicits", JavaCompiler.spliceAfterTransformations(implicits, Seq(MarkOutputGrammar)))
  }

  def getRevealSyntaxSugar: Preset = {
    val implicits = Seq[Particle](DefaultConstructorC, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, FieldDeclarationWithInitializer,
      ConstructorC, ImplicitReturnAtEndOfMethod, IncrementAssignmentC, ForLoopC, LocalDeclarationWithInitializerC,
      ImplicitThisForPrivateMemberSelection, ImplicitJavaLangImport)

    new Preset("Reveal Syntax Sugar", JavaCompiler.spliceAfterTransformations(implicits, Seq(MarkOutputGrammar)), "Performs all compiler phases that still maintain a valid Java program.")
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
    compilerParticles.clear()
    for (particle <- preset.particles)
      compilerParticles.addElement(particle)
  }
}

case class Preset(name: String, particles: Seq[Particle], description: String = "") {
  override def toString = name
}
