package application.compilerBuilder

import java.awt.event._
import java.awt.{GridBagConstraints, GridBagLayout}
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import application.StyleSheet
import application.compilerCockpit.MarkOutputGrammar
import core.particles.{CompilerFromParticles, Particle}
import transformations.javaPlus.ExpressionMethodC
import transformations.javac._
import transformations.javac.classes.FieldDeclarationWithInitializer
import transformations.javac.constructor.{ConstructorC, DefaultConstructorC, ImplicitSuperConstructorCall}
import transformations.javac.methods.assignment.IncrementAssignmentC
import transformations.javac.methods.{BlockCompilerC, ImplicitReturnAtEndOfMethod}
import transformations.javac.statements.ForLoopC
import transformations.javac.statements.locals.LocalDeclarationWithInitializerC

object PresetsPanel
{
  def getSimplifiedByteCodePreset = {
    new Preset("Simplified bytecode", new CompilerFromParticles(JavaCompiler.simpleByteCodeTransformations).
      spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(MarkOutputGrammar)),
      "Simplified JVM bytecode.")
  }

  def getJavaToSimplifiedByteCodePreset = {
    new Preset("Java to simplified bytecode",
      JavaCompiler.spliceBeforeTransformations(JavaCompiler.simpleByteCodeTransformations, Seq(MarkOutputGrammar)),
      "Compiles Java into simplified bytecode")
  }
}

class PresetsPanel(selectedParticles: ParticleInstanceList) extends JPanel(new GridBagLayout()) {

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
    //model.addElement(getAddImplicitsPreset)
    model.addElement(getPrettyPrintPreset)
    model.addElement(getFibonacciExpressionMethodPreset)
    model.addElement(getBlockCompilerPreset)
    model.addElement(getRevealSyntaxSugar)
    model.addElement(PresetsPanel.getJavaToSimplifiedByteCodePreset)
    model.addElement(PresetsPanel.getSimplifiedByteCodePreset)
    model.addElement(getByteCodePreset)
    model
  }

  def getJavaCompilerPreset: Preset = {
    new Preset("Java", getJavaCompiler, "Compiles a subset of Java.")
  }

  def getJavaCompiler: Seq[Particle] = {
    JavaCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(MarkOutputGrammar))
  }

  def getPrettyPrintPreset = {
    new Preset("Pretty Print Java", Seq(MarkOutputGrammar) ++ JavaCompiler.javaCompilerTransformations,
      "Performs no transformations. Just parses and prints the Java.")
  }

  def getFibonacciExpressionMethodPreset = {
    new Preset("Java with expression method", Seq(ExpressionMethodC) ++ getJavaCompiler,
      "Allows you to use an expression as the body of a method.")
  }

  def getBlockCompilerPreset = {
    new Preset("Java statement block", Seq(BlockCompilerC) ++ JavaCompiler.javaCompilerTransformations,
      "The program consists only of a single statement block.")
  }

  def getByteCodePreset = {
    new Preset("Basic bytecode", JavaCompiler.byteCodeTransformations,
      "Regular JVM bytecode.")
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

    new Preset("Reveal Syntax Sugar", JavaCompiler.spliceAfterTransformations(implicits, Seq(MarkOutputGrammar)),
      "Performs all compiler phases that still maintain a valid Java program.")
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
    for (particle <- preset.particles)
      selectedParticles.addElement(new ParticleInstance(particle))
  }
}

case class Preset(name: String, particles: Seq[Particle], description: String = "") {
  override def toString = name
}
