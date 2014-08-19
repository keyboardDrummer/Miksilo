package application;

import core.transformation.sillyCodePieces.Injector;

import javax.swing.*;
import java.awt.*;

public class ExampleListCellRenderer extends DefaultListCellRenderer {
    public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        // I know DefaultListCellRenderer always returns a JLabel
        // super setups up all the defaults
        JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);

        // "value" is whatever object you put into the list, you can use it however you want here

        // I'm going to prefix the label text to demonstrate the point

        label.setText(((Injector)value).name());

        return label;

    }
}