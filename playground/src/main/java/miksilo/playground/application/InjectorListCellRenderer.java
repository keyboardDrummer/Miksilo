package miksilo.playground.application;

import miksilo.modularLanguages.core.deltas.Delta;
import miksilo.playground.application.compilerBuilder.DeltaInstance;
import miksilo.playground.application.compilerBuilder.DeltaLabelPainter;
import miksilo.playground.application.compilerCockpit.MarkOutputGrammar$;
import org.jdesktop.swingx.JXList;

import javax.swing.*;
import java.awt.*;

public class InjectorListCellRenderer extends DefaultListCellRenderer {
  private DeltaLabelPainter painter;

  public InjectorListCellRenderer(DeltaLabelPainter painter) {
    this.painter = painter;
  }

  public void setInJXList(JXList list) {
    list.setCellRenderer(this);
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