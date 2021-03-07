package miksilo.playground.application.compilerBuilder;

import org.jdesktop.swingx.JXList;

import javax.swing.*;
import java.awt.event.MouseEvent;

public class ParticleList extends JXList {

    public String getToolTipText(MouseEvent event) {
        int index = this.locationToIndex(event.getPoint());
        ListModel model = this.getModel();
        if (index >= 0)
        {
            return DeltaInstance.DeltaLike(model.getElementAt(index)).getDelta().description();
        }
        else {
            return "";
        }
    }
}
