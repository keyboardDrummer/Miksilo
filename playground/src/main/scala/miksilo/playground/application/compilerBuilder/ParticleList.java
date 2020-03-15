package miksilo.playground.application.compilerBuilder;

import miksilo.modularLanguages.core.deltas.Delta;
import org.jdesktop.swingx.JXList;

import java.awt.event.MouseEvent;

public class ParticleList extends JXList {

    public String getToolTipText(MouseEvent event) {
        var index = this.locationToIndex(event.getPoint());
        var model = this.getModel();
        if (index >= 0)
        {
            return ((Delta)model.getElementAt(index)).description();
        }
        else {
            return "";
        }
    }
}
