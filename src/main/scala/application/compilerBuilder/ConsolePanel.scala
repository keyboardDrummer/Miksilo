package application.compilerBuilder

import java.awt.BorderLayout
import javax.swing.event.{ListDataEvent, ListDataListener}
import javax.swing.{BorderFactory, DefaultListModel, JPanel, JTextArea}

import core.document.Empty
import core.deltas.{Contract, Delta}
import core.responsiveDocument.ResponsiveDocument

import scala.collection.convert.Wrappers.JEnumerationWrapper

class ConsolePanel(val selectedParticles: DeltaInstanceList)  extends JPanel(new BorderLayout()) {

  val console = new JTextArea()
  console.setBorder(BorderFactory.createLoweredBevelBorder())

  setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Console"))
  add(console)

  selectedParticles.addListDataListener(new ListDataListener {
    override def intervalRemoved(e: ListDataEvent): Unit = refreshConsolePanel()

    override def intervalAdded(e: ListDataEvent): Unit = refreshConsolePanel()

    override def contentsChanged(e: ListDataEvent): Unit = refreshConsolePanel()
  })

  def refreshConsolePanel() = {
    val errors = getDependencyErrors(selectedParticles.scalaElements)
    val document = errors.map(e => e.toDocument).fold[ResponsiveDocument](Empty)((a, b) => a %% b)
    console.setText(document.renderString())
  }

  trait DependencyError {
    def toDocument: ResponsiveDocument
  }
  object MissingDependenciesError extends DependencyError {
    override def toDocument: ResponsiveDocument = "Some dependencies are missing."
  }
  case class BadOrderError(dependency: Delta, dependant: Delta) extends DependencyError {
    override def toDocument: ResponsiveDocument = s"Dependency ${dependency.name} should be placed below ${dependant.name}."
  }

  def getDependencyErrors(transformations: Seq[Delta]) : Set[DependencyError] = {
    val allTransformations = transformations.toSet
    var available = Set.empty[Contract]
    var result = Set.empty[DependencyError]
    var badOrderErrors = Map.empty[Delta, BadOrderError]
    for (transformation <- transformations.reverse) {
      transformation.dependencies2.foreach(dependency =>
        if (!available.contains(dependency))
        {
          if (!allTransformations.contains(dependency))
          {
            result += MissingDependenciesError
          } else
          {
            val key = dependency
            val value = BadOrderError(key, transformation)
            if (!badOrderErrors.contains(key))
              badOrderErrors += key -> value
          }
        }
      )
      available += transformation
    }
    result ++= badOrderErrors.values
    result
  }
}
