package scala_utilities

import xml.Node
import java.lang.Long

object xmlHelper {
  /** convenience function that provides implicit conversion from Node to an anonymous type
      with following members:
      <ul>
        <li>getChildOpt  gets the first child node with specified label</li>
        <li>getLabel  finds the first child node with specified label and returns its text value</li>
        <li>getLong  finds the first child node with specified label and returns its Long value</li>
        <li>getBool  finds the first child node with specified label and returns its Boolean value</li>
      </ul> */
  private implicit def node2Label(node:Node) = new {
    /** Only gets the first child */
    def getChildOpt(label:String) = {
      val children = (node \ label)
      if (children.length > 0) Some(children(0))
      else None
    }

    def getChild(label:String) = (node \ label) (0)

    def getLabelOpt(label:String) = getChildOpt(label) map (_ text)
    def getLabel(label:String) = getChild(label) text

    def getLongOpt(label:String) = getLabelOpt(label).map(Long.parseLong(_))
    def getLong(label:String) = Long.parseLong(getLabel(label))

    def getBool(label:String) = getLabel(label) == "true"
    def getBoolOpt(label:String) = getLabelOpt(label) map (_ == "true")

    def getAttrib(name:String) = (node attribute (node namespace, name)).get.apply(0) text
  }
}
