package application;

import application.compilerBuilder.ParticleInstance;
import application.compilerBuilder.ParticleLabelPainter;
import application.compilerCockpit.MarkOutputGrammar;
import application.compilerCockpit.MarkOutputGrammar$;
import core.particles.Particle;

import javax.swing.*;
import java.awt.*;

public class InjectorListCellRenderer extends DefaultListCellRenderer {
  private ParticleLabelPainter painter;

  public InjectorListCellRenderer(ParticleLabelPainter painter) {
    this.painter = painter;
  }

  public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
    JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);

    Particle particle;
    if (value instanceof Particle)
    {
      particle = (Particle)value;
    } else // if (value instanceof ParticleInstance)
    {
      particle = ((ParticleInstance)value).particle();
    }
    label.setText(particle.name());
    if (!isSelected) {
      Color color = painter.isDependency(particle) ? Color.RED
              : painter.isDependant(particle) ? Color.GREEN : getBackground();
      if (particle == MarkOutputGrammar$.MODULE$)
      {
        color = Color.LIGHT_GRAY;
      }
      this.setBackground(color);
    }

    return label;

  }
}