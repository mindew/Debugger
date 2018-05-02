import scala.swing._

import javax.swing.{JScrollBar, BoundedRangeModel}
import java.awt.event.{AdjustmentListener}


object ScrollBar {
  def wrap(c: JScrollBar): ScrollBar = {
    val w = UIElement.cachedWrapper[ScrollBar](c)
    if (w != null) w
    else new ScrollBar { override lazy val peer = c }
  }
}

class ScrollBar extends Component with Orientable.Wrapper with Adjustable.Wrapper {
	override lazy val peer: JScrollBar = new JScrollBar with SuperMixin

	def valueIsAjusting = peer.getValueIsAdjusting
	def valueIsAjusting_=(b : Boolean) = peer.setValueIsAdjusting(b)

	// TODO: can we find a better interface?
	//def setValues(value: Int = this.value, visible: Int = visibleAmount,
	//             min: Int = minimum, max: Int = maximum) =
	//  peer.setValues(value, visible, min, max)
  }
