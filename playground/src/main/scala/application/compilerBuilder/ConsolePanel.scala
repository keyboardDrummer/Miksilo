package application.compilerBuilder

import java.awt.BorderLayout

import javax.swing.event.{ListDataEvent, ListDataListener}
import javax.swing.{BorderFactory, JPanel, JTextArea}
import application.StyleSheet
import core.deltas.{Contract, Delta}
import core.document.Empty
import core.responsiveDocument.ResponsiveDocument

class ConsolePanel(val selectedParticles: DeltaInstanceList)  extends JPanel(new BorderLayout()) {

  val console = new JTextArea()
  console.setBorder(BorderFactory.createLoweredBevelBorder())

  private val titledBorder = BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Console")
  titledBorder.setTitleFont(StyleSheet.defaultFont)
  setBorder(titledBorder)
  add(console)

  selectedParticles.addListDataListener(new ListDataListener {
    override def intervalRemoved(e: ListDataEvent): Unit = refreshConsolePanel()

    override def intervalAdded(e: ListDataEvent): Unit = refreshConsolePanel()

    override def contentsChanged(e: ListDataEvent): Unit = refreshConsolePanel()
  })

  def refreshConsolePanel(): Unit = {
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
  case class BadOrderError(dependency: Contract, dependant: Contract) extends DependencyError {
    override def toDocument: ResponsiveDocument = s"Dependency ${dependency.name} should be placed below ${dependant.name}."
  }

  def getDependencyErrors(deltas: Seq[Delta]) : Set[DependencyError] = {
    val allDeltas = deltas.toSet[Contract]
    var available = Set.empty[Contract]
    var result = Set.empty[DependencyError]
    var badOrderErrors = Map.empty[Contract, BadOrderError]
    for (transformation <- deltas.reverse) {
      transformation.dependencies.foreach(dependency =>
        if (!available.contains(dependency))
        {
          if (!allDeltas.contains(dependency))
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
