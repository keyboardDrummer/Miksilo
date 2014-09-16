package application.compilerBuilder

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{GridBagConstraints, GridBagLayout}
import javax.swing._
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}

import application.StyleSheet
import application.compilerCockpit.PerformCockpitOutputAction
import core.transformation.sillyCodePieces.Injector
import transformations.javac.constructor.{DefaultConstructorC, ImplicitSuperConstructorCall}
import transformations.javac.methods.ImplicitReturnAtEndOfMethod
import transformations.javac.{ImplicitJavaLangImport, ImplicitObjectSuperClass, ImplicitThisInPrivateCalls, JavaCompiler}

class PresetsPanel(compilerParticles: DefaultListModel[Injector]) extends JPanel(new GridBagLayout()) {

  val model = new DefaultListModel[Preset]()
  initialise()

  def initialise() {
    val listConstraints: GridBagConstraints = new GridBagConstraints()
    listConstraints.fill = GridBagConstraints.BOTH
    listConstraints.weightx = 1
    listConstraints.weighty = 1
    listConstraints.gridx = 0
    listConstraints.insets = StyleSheet.defaultInsets

    StyleSheet.setTitleBorder(this, "Presets")
    val presetsList = new JList(model)
    presetsList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)

    model.addElement(getJavaCompilerPreset)
    model.addElement(getAddImplicitsPreset)
    model.addElement(getPrettyPrintPreset)
    add(StyleSheet.getAnyListVisuals(presetsList), listConstraints)

    val buttonConstraints: GridBagConstraints = new GridBagConstraints()
    buttonConstraints.fill = GridBagConstraints.HORIZONTAL
    buttonConstraints.weightx = 1
    buttonConstraints.gridx = 0
    buttonConstraints.insets = StyleSheet.defaultInsets
    val applyButton: JButton = getApplyButton(presetsList)
    add(applyButton, buttonConstraints)
  }

  def getJavaCompilerPreset: Preset = {
    new Preset("Java Compiler", JavaCompiler.javaCompilerTransformations)
  }

  def getPrettyPrintPreset = {
    new Preset("Pretty Print", Seq(PerformCockpitOutputAction) ++ JavaCompiler.javaCompilerTransformations)
  }

  def getAddImplicitsPreset: Preset = {
    val implicits = Seq[Injector](ImplicitJavaLangImport, DefaultConstructorC, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitThisInPrivateCalls, ImplicitReturnAtEndOfMethod)
    val implicitsSet = implicits.toSet
    val transformations = implicits ++ Seq(PerformCockpitOutputAction) ++
      JavaCompiler.javaCompilerTransformations.filter(t => !implicitsSet.contains(t))
    new Preset("Add Implicits", transformations)
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
        compilerParticles.clear()
        for (particle <- presetsList.getSelectedValue.particles)
          compilerParticles.addElement(particle)
      }
    })
    applyButton
  }
}

case class Preset(name: String, particles: Seq[Injector]) {
  override def toString = name
}
