package application;

import application.compilerBuilder.ParticleLabelPainter;
import core.transformation.sillyCodePieces.Injector;

import javax.swing.*;
import java.awt.*;

public class InjectorListCellRenderer extends DefaultListCellRenderer {
  private ParticleLabelPainter painter;

  public InjectorListCellRenderer(ParticleLabelPainter painter) {
    this.painter = painter;
  }

  public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
    JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);

    Injector injector = (Injector) value;
    label.setText(injector.name());
    if (!isSelected) {
      Color color = painter.isDependency(injector) ? Color.RED
              : painter.isDependant(injector) ? Color.GREEN : getBackground();
      this.setBackground(color);
    }

    return label;

  }
}