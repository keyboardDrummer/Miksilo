package application;

import application.compilerBuilder.DeltaInstance;
import application.compilerBuilder.DeltaLabelPainter;
import application.compilerCockpit.MarkOutputGrammar$;
import core.deltas.Delta;

import javax.swing.*;
import java.awt.*;

public class InjectorListCellRenderer extends DefaultListCellRenderer {
  private DeltaLabelPainter painter;

  public InjectorListCellRenderer(DeltaLabelPainter painter) {
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
      delta = ((DeltaInstance)value).delta();
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