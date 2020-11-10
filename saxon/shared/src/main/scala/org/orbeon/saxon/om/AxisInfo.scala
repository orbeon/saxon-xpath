////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*
    // a list for any future cut-and-pasting...
    ANCESTOR
    ANCESTOR_OR_SELF;
    ATTRIBUTE;
    CHILD;
    DESCENDANT;
    DESCENDANT_OR_SELF;
    FOLLOWING;
    FOLLOWING_SIBLING;
    NAMESPACE;
    PARENT;
    PRECEDING;
    PRECEDING_SIBLING;
    SELF;
 */

package org.orbeon.saxon.om

import org.orbeon.saxon.model.PrimitiveUType.PrimitiveUType
import org.orbeon.saxon.model.{PrimitiveUType, Type, UType}
import org.orbeon.saxon.trans.XPathException
import org.orbeon.saxon.z.IntHashMap

//import scala.collection.compat._
import scala.jdk.CollectionConverters._


/**
  * An axis, that is a direction of navigation in the document structure.
  */
object AxisInfo {

  val ANCESTOR: Int = 0

  /**
    * Constant representing the ancestor-or-self axis
    */ /**
    * Constant representing the ancestor-or-self axis
    */
  val ANCESTOR_OR_SELF: Int = 1

  /**
    * Constant representing the attribute axis
    */ /**
    * Constant representing the attribute axis
    */
  val ATTRIBUTE: Int = 2

  /**
    * Constant representing the child axis
    */ /**
    * Constant representing the child axis
    */
  val CHILD: Int = 3

  /**
    * Constant representing the descendant axis
    */ /**
    * Constant representing the descendant axis
    */
  val DESCENDANT: Int = 4

  /**
    * Constant representing the descendant-or-self axis
    */ /**
    * Constant representing the descendant-or-self axis
    */
  val DESCENDANT_OR_SELF: Int = 5

  /**
    * Constant representing the following axis
    */ /**
    * Constant representing the following axis
    */
  val FOLLOWING: Int = 6

  /**
    * Constant representing the following-sibling axis
    */ /**
    * Constant representing the following-sibling axis
    */
  val FOLLOWING_SIBLING: Int = 7

  /**
    * Constant representing the namespace axis
    */ /**
    * Constant representing the namespace axis
    */
  val NAMESPACE: Int = 8

  /**
    * Constant representing the parent axis
    */ /**
    * Constant representing the parent axis
    */
  val PARENT: Int = 9

  /**
    * Constant representing the preceding axis
    */ /**
    * Constant representing the preceding axis
    */
  val PRECEDING: Int = 10

  /**
    * Constant representing the preceding-sibling axis
    */ /**
    * Constant representing the preceding-sibling axis
    */
  val PRECEDING_SIBLING: Int = 11

  /**
    * Constant representing the self axis
    */ /**
    * Constant representing the self axis
    */
  val SELF: Int = 12

  /**
    * Constant representing the preceding-or-ancestor axis. This axis is used internally by the xsl:number implementation, it returns the union of the preceding axis and the ancestor axis.
    */ /**
    * Constant representing the preceding-or-ancestor axis. This axis is used internally by the xsl:number implementation, it returns the union of the preceding axis and the ancestor axis.
    */
  val PRECEDING_OR_ANCESTOR: Int = 13

  val principalNodeType: Array[Short] = Array( // ANCESTOR
    Type.ELEMENT, // ANCESTOR_OR_SELF;
    Type.ELEMENT, // ATTRIBUTE;
    Type.ATTRIBUTE, // CHILD;
    Type.ELEMENT, // DESCENDANT;
    Type.ELEMENT, // DESCENDANT_OR_SELF;
    Type.ELEMENT, // FOLLOWING;
    Type.ELEMENT, // FOLLOWING_SIBLING;
    Type.ELEMENT, // NAMESPACE;
    Type.NAMESPACE, // PARENT;
    Type.ELEMENT, // PRECEDING;
    Type.ELEMENT, // PRECEDING_SIBLING;
    Type.ELEMENT, // SELF;
    Type.ELEMENT, // PRECEDING_OR_ANCESTOR;
    Type.ELEMENT
  )

  val principalNodeUType: Array[UType] = Array( // ANCESTOR
    UType.ELEMENT, // ANCESTOR_OR_SELF;
    UType.ELEMENT, // ATTRIBUTE;
    UType.ATTRIBUTE, // CHILD;
    UType.ELEMENT, // DESCENDANT;
    UType.ELEMENT, // DESCENDANT_OR_SELF;
    UType.ELEMENT, // FOLLOWING;
    UType.ELEMENT, // FOLLOWING_SIBLING;
    UType.ELEMENT, // NAMESPACE;
    UType.NAMESPACE, // PARENT;
    UType.ELEMENT, // PRECEDING;
    UType.ELEMENT, // PRECEDING_SIBLING;
    UType.ELEMENT, // SELF;
    UType.ELEMENT, // PRECEDING_OR_ANCESTOR;
    UType.ELEMENT
  )

  val isForwards: Array[Boolean] = Array(false, false, true, true, true, true,
    true, true, true, true, false, false, true, false)

  val isPeerAxis: Array[Boolean] = Array(false, false, true, true, false,
    false, false, true, true, true, false, true, true, false)

  val isSubtreeAxis: Array[Boolean] = Array(false, false, true, true, true,
    true, false, false, true, false, false, false, true, false)

  val axisName: Array[String] = Array(
    "ancestor",
    "ancestor-or-self",
    "attribute",
    "child",
    "descendant",
    "descendant-or-self",
    "following",
    "following-sibling",
    "namespace",
    "parent",
    "preceding",
    "preceding-sibling",
    "self",
    "preceding-or-ancestor"
  )

  def getAxisNumber(name: String): Int = name match {
    case "ancestor" => ANCESTOR
    case "ancestor-or-self" => ANCESTOR_OR_SELF
    case "attribute" => ATTRIBUTE
    case "child" => CHILD
    case "descendant" => DESCENDANT
    case "descendant-or-self" => DESCENDANT_OR_SELF
    case "following" => FOLLOWING
    case "following-sibling" => FOLLOWING_SIBLING
    case "namespace" => NAMESPACE
    case "parent" => PARENT
    case "preceding" => PRECEDING
    case "preceding-sibling" => PRECEDING_SIBLING
    case "self" => SELF
    case "preceding-or-ancestor" => PRECEDING_OR_ANCESTOR
// preceding-or-ancestor cannot be used in an XPath expression
    case _ => throw new XPathException("Unknown axis name: " + name)

  }

  private val DOC: Int = 1 << Type.DOCUMENT
  private val ELE: Int = 1 << Type.ELEMENT
  private val ATT: Int = 1 << Type.ATTRIBUTE
  private val TEX: Int = 1 << Type.TEXT
  private val PIN: Int = 1 << Type.PROCESSING_INSTRUCTION
  private val COM: Int = 1 << Type.COMMENT
  private val NAM: Int = 1 << Type.NAMESPACE

  private val voidAxisTable: Array[Int] = Array(
    DOC,
    0, // ATTRIBUTE;
    DOC | ATT | TEX | PIN | COM | NAM, // CHILD;
    ATT | TEX | PIN | COM | NAM, // DESCENDANT;
    ATT | TEX | PIN | COM | NAM,
    0,
    DOC, // FOLLOWING_SIBLING;
    DOC | ATT | NAM, // NAMESPACE;
    DOC | ATT | TEX | PIN | COM | NAM,
    DOC,
    DOC, // PRECEDING_SIBLING;
    DOC | ATT | NAM,
    0
  )

  def isAlwaysEmpty(axis: Int, nodeKind: Int): Boolean =
    (voidAxisTable(axis) & (1 << nodeKind)) != 0

  private val nodeKindTable: Array[Int] = Array( // ANCESTOR
    DOC | ELE, // ANCESTOR_OR_SELF;
    DOC | ELE | ATT | TEX | PIN | COM | NAM,
    ATT, // CHILD;
    ELE | TEX | PIN | COM, // DESCENDANT;
    ELE | TEX | PIN | COM, // DESCENDANT_OR_SELF;
    DOC | ELE | ATT | TEX | PIN | COM | NAM, // FOLLOWING;
    ELE | TEX | PIN | COM, // FOLLOWING_SIBLING;
    ELE | TEX | PIN | COM,
    NAM, // PARENT;
    DOC | ELE, // PRECEDING;
    ELE | TEX | PIN | COM, // PRECEDING_SIBLING;
    ELE | TEX | PIN | COM, // SELF;
    DOC | ELE | ATT | TEX | PIN | COM | NAM
  )

  def containsNodeKind(axis: Int, nodeKind: Int): Boolean =
    nodeKind == Type.NODE || (nodeKindTable(axis) & (1 << nodeKind)) != 0

  val inverseAxis: Array[Int] = Array(DESCENDANT,
    DESCENDANT_OR_SELF,
    PARENT,
    PARENT,
    ANCESTOR,
    ANCESTOR_OR_SELF,
    PRECEDING,
    PRECEDING_SIBLING,
    PARENT,
    CHILD,
    FOLLOWING,
    FOLLOWING_SIBLING,
    SELF)

  val excludeSelfAxis: Array[Int] = Array(ANCESTOR,
    ANCESTOR,
    ATTRIBUTE,
    CHILD,
    DESCENDANT,
    DESCENDANT,
    FOLLOWING,
    FOLLOWING_SIBLING,
    NAMESPACE,
    PARENT,
    PRECEDING,
    PRECEDING_SIBLING,
    SELF)

  private val axisTransitions: IntHashMap[UType] = new IntHashMap(50)

  private def e(origin: PrimitiveUType, axis: Int, target: UType): Unit =
    axisTransitions.put(makeKey(origin, axis), target)

  private def makeKey(origin: PrimitiveUType, axis: Int): Int =
    origin.getBit << 16 | axis

  def getTargetUType(origin: UType, axis: Int): UType = {
    var resultType = UType.VOID
    val origins = origin.intersection(UType.ANY_NODE).decompose()
    for (u <- origins.asScala) {
      val r = axisTransitions.get(makeKey(u, axis))
      if (r == null) {
        System.err.println(
          "Unknown transitions for primitive type " + u.toString +
            "::" +
            axis)
      }
      resultType = resultType.union(r)
    }
    resultType
  }

  e(PrimitiveUType.DOCUMENT,  ANCESTOR,           UType.VOID)
  e(PrimitiveUType.DOCUMENT,  ANCESTOR_OR_SELF,   UType.DOCUMENT)
  e(PrimitiveUType.DOCUMENT,  ATTRIBUTE,          UType.VOID)
  e(PrimitiveUType.DOCUMENT,  CHILD,              UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.DOCUMENT,  DESCENDANT,         UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.DOCUMENT,  DESCENDANT_OR_SELF, UType.DOCUMENT.union(UType.CHILD_NODE_KINDS))
  e(PrimitiveUType.DOCUMENT,  FOLLOWING,          UType.VOID)
  e(PrimitiveUType.DOCUMENT,  FOLLOWING_SIBLING,  UType.VOID)
  e(PrimitiveUType.DOCUMENT,  NAMESPACE,          UType.VOID)
  e(PrimitiveUType.DOCUMENT,  PARENT,             UType.VOID)
  e(PrimitiveUType.DOCUMENT,  PRECEDING,          UType.VOID)
  e(PrimitiveUType.DOCUMENT,  PRECEDING_SIBLING,  UType.VOID)
  e(PrimitiveUType.DOCUMENT,  SELF,               UType.DOCUMENT)
  e(PrimitiveUType.ELEMENT,   ANCESTOR,           UType.PARENT_NODE_KINDS)
  e(PrimitiveUType.ELEMENT,   ANCESTOR_OR_SELF,   UType.PARENT_NODE_KINDS)
  e(PrimitiveUType.ELEMENT,   ATTRIBUTE,          UType.ATTRIBUTE)
  e(PrimitiveUType.ELEMENT,   CHILD,              UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.ELEMENT,   DESCENDANT,         UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.ELEMENT,   DESCENDANT_OR_SELF, UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.ELEMENT,   FOLLOWING,          UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.ELEMENT,   FOLLOWING_SIBLING,  UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.ELEMENT,   NAMESPACE,          UType.NAMESPACE)
  e(PrimitiveUType.ELEMENT,   PARENT,             UType.PARENT_NODE_KINDS)
  e(PrimitiveUType.ELEMENT,   PRECEDING,          UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.ELEMENT,   PRECEDING_SIBLING,  UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.ELEMENT,   SELF,               UType.ELEMENT)
  e(PrimitiveUType.ATTRIBUTE, ANCESTOR,           UType.PARENT_NODE_KINDS)
  e(PrimitiveUType.ATTRIBUTE, ANCESTOR_OR_SELF,   UType.ATTRIBUTE.union(UType.PARENT_NODE_KINDS))
  e(PrimitiveUType.ATTRIBUTE, ATTRIBUTE,          UType.VOID)
  e(PrimitiveUType.ATTRIBUTE, CHILD,              UType.VOID)
  e(PrimitiveUType.ATTRIBUTE, DESCENDANT,         UType.VOID)
  e(PrimitiveUType.ATTRIBUTE, DESCENDANT_OR_SELF, UType.ATTRIBUTE)
  e(PrimitiveUType.ATTRIBUTE, FOLLOWING,          UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.ATTRIBUTE, FOLLOWING_SIBLING,  UType.VOID)
  e(PrimitiveUType.ATTRIBUTE, NAMESPACE,          UType.VOID)
  e(PrimitiveUType.ATTRIBUTE, PARENT,             UType.ELEMENT)
  e(PrimitiveUType.ATTRIBUTE, PRECEDING,          UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.ATTRIBUTE, PRECEDING_SIBLING,  UType.VOID)
  e(PrimitiveUType.ATTRIBUTE, SELF,               UType.ATTRIBUTE)
  e(PrimitiveUType.TEXT,      ANCESTOR,           UType.PARENT_NODE_KINDS)
  e(PrimitiveUType.TEXT,      ANCESTOR_OR_SELF,   UType.TEXT.union(UType.PARENT_NODE_KINDS))
  e(PrimitiveUType.TEXT,      ATTRIBUTE,          UType.VOID)
  e(PrimitiveUType.TEXT,      CHILD,              UType.VOID)
  e(PrimitiveUType.TEXT,      DESCENDANT,         UType.VOID)
  e(PrimitiveUType.TEXT,      DESCENDANT_OR_SELF, UType.TEXT)
  e(PrimitiveUType.TEXT,      FOLLOWING,          UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.TEXT,      FOLLOWING_SIBLING,  UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.TEXT,      NAMESPACE,          UType.VOID)
  e(PrimitiveUType.TEXT,      PARENT,             UType.PARENT_NODE_KINDS)
  e(PrimitiveUType.TEXT,      PRECEDING,          UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.TEXT,      PRECEDING_SIBLING,  UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.TEXT,      SELF,               UType.TEXT)
  e(PrimitiveUType.PI,        ANCESTOR,           UType.PARENT_NODE_KINDS)
  e(PrimitiveUType.PI,        ANCESTOR_OR_SELF,   UType.PI.union(UType.PARENT_NODE_KINDS))
  e(PrimitiveUType.PI,        ATTRIBUTE,          UType.VOID)
  e(PrimitiveUType.PI,        CHILD,              UType.VOID)
  e(PrimitiveUType.PI,        DESCENDANT,         UType.VOID)
  e(PrimitiveUType.PI,        DESCENDANT_OR_SELF, UType.PI)
  e(PrimitiveUType.PI,        FOLLOWING,          UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.PI,        FOLLOWING_SIBLING,  UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.PI,        NAMESPACE,          UType.VOID)
  e(PrimitiveUType.PI,        PARENT,             UType.PARENT_NODE_KINDS)
  e(PrimitiveUType.PI,        PRECEDING,          UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.PI,        PRECEDING_SIBLING,  UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.PI,        SELF,               UType.PI)
  e(PrimitiveUType.COMMENT,   ANCESTOR,           UType.PARENT_NODE_KINDS)
  e(PrimitiveUType.COMMENT,   ANCESTOR_OR_SELF,   UType.COMMENT.union(UType.PARENT_NODE_KINDS))
  e(PrimitiveUType.COMMENT,   ATTRIBUTE,          UType.VOID)
  e(PrimitiveUType.COMMENT,   CHILD,              UType.VOID)
  e(PrimitiveUType.COMMENT,   DESCENDANT,         UType.VOID)
  e(PrimitiveUType.COMMENT,   DESCENDANT_OR_SELF, UType.COMMENT)
  e(PrimitiveUType.COMMENT,   FOLLOWING,          UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.COMMENT,   FOLLOWING_SIBLING,  UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.COMMENT,   NAMESPACE,          UType.VOID)
  e(PrimitiveUType.COMMENT,   PARENT,             UType.PARENT_NODE_KINDS)
  e(PrimitiveUType.COMMENT,   PRECEDING,          UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.COMMENT,   PRECEDING_SIBLING,  UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.COMMENT,   SELF,               UType.COMMENT)
  e(PrimitiveUType.NAMESPACE, ANCESTOR,           UType.PARENT_NODE_KINDS)
  e(PrimitiveUType.NAMESPACE, ANCESTOR_OR_SELF,   UType.NAMESPACE.union(UType.PARENT_NODE_KINDS))
  e(PrimitiveUType.NAMESPACE, ATTRIBUTE,          UType.VOID)
  e(PrimitiveUType.NAMESPACE, CHILD,              UType.VOID)
  e(PrimitiveUType.NAMESPACE, DESCENDANT,         UType.VOID)
  e(PrimitiveUType.NAMESPACE, DESCENDANT_OR_SELF, UType.NAMESPACE)
  e(PrimitiveUType.NAMESPACE, FOLLOWING,          UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.NAMESPACE, FOLLOWING_SIBLING,  UType.VOID)
  e(PrimitiveUType.NAMESPACE, NAMESPACE,          UType.VOID)
  e(PrimitiveUType.NAMESPACE, PARENT,             UType.ELEMENT)
  e(PrimitiveUType.NAMESPACE, PRECEDING,          UType.CHILD_NODE_KINDS)
  e(PrimitiveUType.NAMESPACE, PRECEDING_SIBLING,  UType.VOID)
  e(PrimitiveUType.NAMESPACE, SELF,               UType.NAMESPACE)
}
