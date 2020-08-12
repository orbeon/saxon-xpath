package net.sf.saxon.event

import net.sf.saxon.s9api.Action

import scala.jdk.CollectionConverters._

import java.util.List

class CloseNotifier(next: Receiver, private var actionList: List[Action])
  extends ProxyReceiver(next) {

  override def close(): Unit = {
    super.close()
    if (actionList != null) {
      for (action <- actionList.asScala) {
        action.act()
      }
    }
  }

}
