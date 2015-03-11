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
import transformations.javac.constructor.{DefaultConstructorC, ImplicitSuperConstructorCall}
import transformations.javac.methods.{BlockCompilerC, ImplicitReturnAtEndOfMethod}

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
    val presetsList = new JList(model)
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
      ImplicitObjectSuperClass, ImplicitThisInPrivateCalls, ImplicitReturnAtEndOfMethod)

    new Preset("Reveal Java Implicits", JavaCompiler.spliceAfterTransformations(implicits, Seq(MarkOutputGrammar)))
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

case class Preset(name: String, particles: Seq[Particle]) {
  override def toString = name
}
