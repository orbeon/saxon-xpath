package net.sf.saxon.s9api.streams

import net.sf.saxon.lib.ConversionRules

import net.sf.saxon.ma.arrays.ArrayItem

import net.sf.saxon.model.AtomicType

import net.sf.saxon.model.Converter

import net.sf.saxon.model.Type

import net.sf.saxon.model.ValidationException

import net.sf.saxon.om.AtomicSequence

import net.sf.saxon.om.NameChecker

import net.sf.saxon.om.NodeInfo

import net.sf.saxon.pattern.NodeTest

import net.sf.saxon.s9api._

import net.sf.saxon.trans.XPathException

import net.sf.saxon.tree.iter.AtomicIterator

import net.sf.saxon.value.AtomicValue

import net.sf.saxon.value.StringValue

import net.sf.saxon.value.Whitespace

import java.util.ArrayList

import java.util.Arrays

import java.util.List

import java.util.function.Predicate

import java.util.stream.Stream

import net.sf.saxon.s9api.streams.Predicates._

object Steps {

  def root(): Step[XdmNode] = new Step[XdmNode]() {
    def apply(origin: XdmItem): Stream[XdmNode] =
      if (origin.isInstanceOf[XdmNode])
        Stream.of(origin.asInstanceOf[XdmNode].getRoot)
      else Stream.empty()
  }

  def atomize(): Step[XdmAtomicValue] = new Step[XdmAtomicValue]() {
    override def apply(item: XdmItem): Stream[_ <: XdmAtomicValue] =
      if (item.isInstanceOf[XdmAtomicValue]) {
        Stream.of(item.asInstanceOf[XdmAtomicValue])
      } else if (item.isInstanceOf[XdmNode]) {
        item
          .asInstanceOf[XdmNode]
          .getTypedValue
          .stream()
          .asInstanceOf[XdmStream[XdmAtomicValue]]
      } else if (item.isInstanceOf[XdmArray]) {
        val arrayItem: ArrayItem =
          item.asInstanceOf[XdmArray].getUnderlyingValue
        val data: AtomicSequence = arrayItem.atomize()
        XdmValue.wrap(data).stream().asInstanceOf[XdmStream[XdmAtomicValue]]
      } else {
        throw new SaxonApiUncheckedException(
          new SaxonApiException("Cannot atomize supplied value"))
      }
  }

  def castAs(`type`: ItemType): Step[XdmAtomicValue] = {
    if (!ItemType.ANY_ATOMIC_VALUE.subsumes(`type`)) {
      throw new IllegalArgumentException(
        "Target of castAs must be an atomic type")
    }
    val tType: net.sf.saxon.model.ItemType =
      `type`.getUnderlyingItemType.getPrimitiveItemType
    val rules: ConversionRules = `type`.getConversionRules
    atomize().thenDo(new Step[XdmAtomicValue]() {
      def apply(xdmItem: XdmItem): Stream[_ <: XdmAtomicValue] = {
        val source: AtomicValue =
          xdmItem.asInstanceOf[XdmAtomicValue].getUnderlyingValue
        val converter: Converter =
          rules.getConverter(source.getItemType,
            tType.asInstanceOf[AtomicType])
        val result: AtomicValue = converter.convert(source).asAtomic()
        Stream.of(XdmValue.wrap(result).asInstanceOf[XdmAtomicValue])
      }
    })
  }

  private val ANCESTOR: Step[XdmNode] = new AxisStep(Axis.ANCESTOR)

  private val ANCESTOR_OR_SELF: Step[XdmNode] = new AxisStep(
    Axis.ANCESTOR_OR_SELF)

  private val ATTRIBUTE: Step[XdmNode] = new AxisStep(Axis.ATTRIBUTE)

  private val CHILD: Step[XdmNode] = new AxisStep(Axis.CHILD)

  private val DESCENDANT: Step[XdmNode] = new AxisStep(Axis.DESCENDANT)

  private val DESCENDANT_OR_SELF: Step[XdmNode] = new AxisStep(
    Axis.DESCENDANT_OR_SELF)

  private val FOLLOWING: Step[XdmNode] = new AxisStep(Axis.FOLLOWING)

  private val FOLLOWING_SIBLING: Step[XdmNode] = new AxisStep(
    Axis.FOLLOWING_SIBLING)

  private val NAMESPACE: Step[XdmNode] = new AxisStep(Axis.NAMESPACE)

  private val PARENT: Step[XdmNode] = new AxisStep(Axis.PARENT)

  private val PRECEDING_SIBLING: Step[XdmNode] = new AxisStep(
    Axis.PRECEDING_SIBLING)

  private val PRECEDING: Step[XdmNode] = new AxisStep(Axis.PRECEDING)

  private val SELF: Step[XdmNode] = new AxisStep(Axis.SELF)

  def nothing[U <: XdmItem](): Step[U] = new Step[U]() {
    def apply(xdmItem: XdmItem): Stream[U] = Stream.empty()
  }

  private def nodeTestPredicate(test: NodeTest): Predicate[XdmNode] =
    (item) => test.test(item.getUnderlyingNode)

  private def localNamePredicate(given: String): Predicate[_ >: XdmNode] =
    if ("*" == given) {
      isElement
    } else { (item) => {
      val node: NodeInfo = item.getUnderlyingNode
      node.getNodeKind == Type.ELEMENT && node.getLocalPart == given
    }
    }

  private def expandedNamePredicate(ns: String,
                                    local: String): Predicate[_ >: XdmNode] =
    (item) => {
      val node: NodeInfo = item.getUnderlyingNode
      node.getNodeKind == Type.ELEMENT && node.getLocalPart == local &&
        node.getURI == ns
    }

  def ancestor(): Step[XdmNode] = ANCESTOR

  def ancestor(localName: String): Step[XdmNode] =
    ancestor().where(localNamePredicate(localName))

  def ancestor(uri: String, localName: String): Step[XdmNode] =
    ancestor().where(expandedNamePredicate(uri, localName))

  def ancestor(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    ancestor().where(filter)

  def ancestorOrSelf(): Step[XdmNode] = ANCESTOR_OR_SELF

  def ancestorOrSelf(localName: String): Step[XdmNode] =
    ancestorOrSelf().where(localNamePredicate(localName))

  def ancestorOrSelf(uri: String, localName: String): Step[XdmNode] =
    ancestorOrSelf().where(expandedNamePredicate(uri, localName))

  def ancestorOrSelf(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    ancestorOrSelf().where(filter)

  def attribute(): Step[XdmNode] = ATTRIBUTE

  def attribute(localName: String): Step[XdmNode] =
    if ("*" == localName) attribute()
    else attribute().where(hasLocalName(localName))

  def attribute(uri: String, localName: String): Step[XdmNode] =
    attribute().where(hasName(uri, localName))

  def attribute(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    attribute().where(filter)

  def child(): Step[XdmNode] = CHILD

  def child(localName: String): Step[XdmNode] =
    child().where(localNamePredicate(localName))

  def child(uri: String, localName: String): Step[XdmNode] =
    child().where(expandedNamePredicate(uri, localName))

  def child(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    child().where(filter)

  def descendant(): Step[XdmNode] = DESCENDANT

  def descendant(localName: String): Step[XdmNode] =
    descendant().where(localNamePredicate(localName))

  def descendant(uri: String, localName: String): Step[XdmNode] =
    descendant().where(expandedNamePredicate(uri, localName))

  def descendantOrSelf(): Step[XdmNode] = DESCENDANT_OR_SELF

  def descendant(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    descendant().where(filter)

  def descendantOrSelf(localName: String): Step[XdmNode] =
    descendantOrSelf().where(localNamePredicate(localName))

  def descendantOrSelf(uri: String, localName: String): Step[XdmNode] =
    descendantOrSelf().where(expandedNamePredicate(uri, localName))

  def descendantOrSelf(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    descendantOrSelf().where(filter)

  def following(): Step[XdmNode] = FOLLOWING

  def following(localName: String): Step[XdmNode] =
    following().where(localNamePredicate(localName))

  def following(uri: String, localName: String): Step[XdmNode] =
    following().where(expandedNamePredicate(uri, localName))

  def following(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    following().where(filter)

  def followingSibling(): Step[XdmNode] = FOLLOWING_SIBLING

  def followingSibling(localName: String): Step[XdmNode] =
    followingSibling().where(localNamePredicate(localName))

  def followingSibling(uri: String, localName: String): Step[XdmNode] =
    followingSibling().where(expandedNamePredicate(uri, localName))

  def followingSibling(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    followingSibling().where(filter)

  def namespace(): Step[XdmNode] = NAMESPACE

  def namespace(localName: String): Step[XdmNode] =
    if ("*" == localName) namespace()
    else namespace().where(hasLocalName(localName))

  def namespace(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    namespace().where(filter)

  def parent(): Step[XdmNode] = PARENT

  def parent(localName: String): Step[XdmNode] =
    parent().where(isElement).where(localNamePredicate(localName))

  def parent(uri: String, localName: String): Step[XdmNode] =
    parent().where(expandedNamePredicate(uri, localName))

  def parent(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    parent().where(filter)

  def precedingSibling(): Step[XdmNode] = PRECEDING_SIBLING

  def precedingSibling(localName: String): Step[XdmNode] =
    precedingSibling().where(localNamePredicate(localName))

  def precedingSibling(uri: String, localName: String): Step[XdmNode] =
    precedingSibling().where(expandedNamePredicate(uri, localName))

  def precedingSibling(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    precedingSibling().where(filter)

  def preceding(): Step[XdmNode] = PRECEDING

  def preceding(localName: String): Step[XdmNode] =
    preceding().where(localNamePredicate(localName))

  def preceding(uri: String, localName: String): Step[XdmNode] =
    preceding().where(expandedNamePredicate(uri, localName))

  def preceding(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    preceding().where(filter)

  def self(): Step[XdmNode] = SELF

  def self(localName: String): Step[XdmNode] =
    self().where(localNamePredicate(localName))

  def self(uri: String, localName: String): Step[XdmNode] =
    self().where(expandedNamePredicate(uri, localName))

  def self(filter: Predicate[_ >: XdmNode]): Step[XdmNode] =
    self().where(filter)

  def text(): Step[XdmNode] = child().where(isText)

  @SafeVarargs
  def path1(steps: Step[_ <: XdmNode]*): Step[_ <: XdmNode] =
    pathFromList(Arrays.asList(steps: _*))

  private def pathFromList(steps: List[Step[_ <: XdmNode]]): Step[_ <: XdmNode] =
    if (steps.isEmpty) {
      nothing()
    } else if (steps.size == 1) {
      steps.get(0)
    } else {
      steps.get(0).thenDo(pathFromList(steps.subList(1, steps.size)))
    }

  def path(steps: String*): Step[_ <: XdmNode] = {
    val pathSteps: List[Step[_ <: XdmNode]] =
      new ArrayList[Step[_ <: XdmNode]]()
    for (step <- steps) {
      if (step.==("/")) {
        pathSteps.add(root().where(isDocument))
      } else if (step.==("..")) {
        pathSteps.add(parent())
      } else if (step.==("*")) {
        pathSteps.add(child(isElement))
      } else if (step.==("//")) {
        pathSteps.add(descendantOrSelf())
      } else if (step.startsWith("@")) {
        val name: String = step.substring(1)
        if (!NameChecker.isValidNCName(name)) {
          throw new IllegalArgumentException("Invalid attribute name " + name)
        }
        pathSteps.add(attribute(name))
      } else {
        if (!NameChecker.isValidNCName(step)) {
          throw new IllegalArgumentException("Invalid element name " + step)
        }
        pathSteps.add(child(step))
      }
    }
    pathFromList(pathSteps)
  }

  def tokenize(): Step[XdmAtomicValue] = new Step[XdmAtomicValue]() {
    override def apply(item: XdmItem): Stream[_ <: XdmAtomicValue] = {
      val iter: AtomicIterator[StringValue] =
        new Whitespace.Tokenizer(item.getStringValue)
      XdmSequenceIterator.ofAtomicValues(iter).asInstanceOf[XdmAtomicValue].stream().asInstanceOf[Stream[_ <: XdmAtomicValue]]
    }
  }

  def id(doc: XdmNode): Step[XdmNode] = new Step[XdmNode]() {
    override def apply(item: XdmItem): Stream[XdmNode] = {
      if (doc.getNodeKind != XdmNodeKind.DOCUMENT) {
        throw new IllegalArgumentException(
          "id() - argument is not a document node")
      }
      XdmNodeKind
      val target: NodeInfo =
        doc.getUnderlyingNode.getTreeInfo.selectID(item.getStringValue, getParent = true)
      if (target == null) Stream.empty()
      else Stream.of(XdmValue.wrap(target).asInstanceOf[XdmNode])
    }
  }

}

