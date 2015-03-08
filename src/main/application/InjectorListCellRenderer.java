package application;

import application.compilerBuilder.ParticleLabelPainter;
import core.transformation.Particle;

import javax.swing.*;
import java.awt.*;

public class InjectorListCellRenderer extends DefaultListCellRenderer {
  private ParticleLabelPainter painter;

  public InjectorListCellRenderer(ParticleLabelPainter painter) {
    this.painter = painter;
  }

  public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
    JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);

    Particle particle = (Particle) value;
    label.setText(particle.name());
    if (!isSelected) {
      Color color = painter.isDependency(particle) ? Color.RED
              : painter.isDependant(particle) ? Color.GREEN : getBackground();
      this.setBackground(color);
    }

    return label;

  }
}