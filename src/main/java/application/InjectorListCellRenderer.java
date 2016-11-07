package application;

import application.compilerBuilder.ParticleInstance;
import application.compilerBuilder.ParticleLabelPainter;
import application.compilerCockpit.MarkOutputGrammar$;
import core.particles.Delta;

import javax.swing.*;
import java.awt.*;

public class InjectorListCellRenderer extends DefaultListCellRenderer {
  private ParticleLabelPainter painter;

  public InjectorListCellRenderer(ParticleLabelPainter painter) {
    this.painter = painter;
  }

  public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
    JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);

    Delta delta;
    if (value instanceof Delta)
    {
      delta = (Delta)value;
    } else // if (value instanceof ParticleInstance)
    {
      delta = ((ParticleInstance)value).particle();
    }
    label.setText(delta.name());
    if (!isSelected) {
      Color color = painter.isDependency(delta) ? Color.RED
              : painter.isDependant(delta) ? Color.GREEN : getBackground();
      if (delta == MarkOutputGrammar$.MODULE$)
      {
        color = Color.LIGHT_GRAY;
      }
      this.setBackground(color);
    }

    return label;

  }
}