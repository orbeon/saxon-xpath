////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.event

import net.sf.saxon.model.SchemaType

import net.sf.saxon.model.Type

import net.sf.saxon.om._

import net.sf.saxon.s9api.Location

import net.sf.saxon.trans.XPathException

import java.util.HashMap

import java.util.Map

import java.util.Stack

import RegularSequenceChecker._


object RegularSequenceChecker {

  object State extends Enumeration {
    type State = Value
    val Initial = Value("Initial")
    val Open = Value("Open")
    val StartTag = Value("StartTag")
    val Content = Value("Content")
    val Final = Value("Final")
    val Failed = Value("Failed")

  }

  object Transition extends Enumeration {

    val OPEN: Transition = new Transition()

    val APPEND: Transition = new Transition()

    val TEXT: Transition = new Transition()

    val COMMENT: Transition = new Transition()

    val PI: Transition = new Transition()

    val START_DOCUMENT: Transition = new Transition()

    val START_ELEMENT: Transition = new Transition()

    val END_ELEMENT: Transition = new Transition()

    val END_DOCUMENT: Transition = new Transition()

    val CLOSE: Transition = new Transition()

    class Transition extends Val

    implicit def convertValue(v: Value): Transition =
      v.asInstanceOf[Transition]

  }

  private var machine: Map[State.State, Map[Transition.Transition, State.State]] = new HashMap()

  private def edge(from: State.State, event: Transition.Transition, to: State.State): Unit = {
    val edges: Map[Transition.Transition, State.State] =
      machine.computeIfAbsent(from, (s) => new HashMap())
    edges.put(event, to)
  }

  edge(State.Initial, Transition.OPEN, State.Open)

  edge(State.Open, Transition.APPEND, State.Open)

  edge(State.Open, Transition.TEXT, State.Open)

  edge(State.Open, Transition.COMMENT, State.Open)

  edge(State.Open, Transition.PI, State.Open)

  edge(State.Open, Transition.START_DOCUMENT, State.Content)

  edge(State.Open, Transition.START_ELEMENT, State.Content)

  edge(State.Content, Transition.TEXT, State.Content)

  edge(State.Content, Transition.COMMENT, State.Content)

  edge(State.Content, Transition.PI, State.Content)

  edge(State.Content, Transition.START_ELEMENT, State.Content)

  // or Open if the stack is empty
  edge(State.Content, Transition.END_ELEMENT, State.Content)

  edge(State.Content, Transition.END_DOCUMENT, State.Open)

  edge(State.Open, Transition.CLOSE, State.Final)

  edge(State.Failed, Transition.CLOSE, State.Failed)

}

/**
 * A <tt>RegularSequenceChecker</tt> is a filter that can be inserted into a Receiver pipeline
 * to check that the sequence of events passed in is a <b>regular event sequence</b>. Many
 * (though not all) implementations of {@link Outputter} require the sequence of events to
 * be regular according to this definition.
 * <p>A sequence of {@code Receiver} events is <b>regular</b> if the following conditions
 * are satisfied:</p>
 * <ol>
 * <li>Calls to {@link Outputter#startElement(NodeName, SchemaType, Location, int)}, {@link #endElement()},
 * {@link Outputter#startDocument(int)}, and {@link #endDocument()} are properly paired and nested.</li>
 * <li>Events must only occur in a state where they are permitted; the states and transitions
 * between states are defined by the table below. The initial state is <b>initial</b>,
 * and the final state must be <b>final</b>.</li>
 * </ol>
 * <table>
 * <thead>
 * <tr><td>State</td><td>Events</td><td>Next State</td></tr>
 * </thead>
 * <tbody>
 * <tr><td>initial</td><td>{@link #open()}</td><td>open</td></tr>
 * <tr><td>open</td><td>{@link #open()}</td><td>open</td></tr>
 * <tr><td>open</td><td>{@link Outputter#append(Item, Location, int)}, {@link #append(Item)},
 * {@link Outputter#characters(CharSequence, Location, int)}, {@link Outputter#comment(CharSequence, Location, int)},
 * {@link Outputter#processingInstruction(String, CharSequence, Location, int)}</td><td>open</td></tr>
 * <tr><td>open</td><td>{@link Outputter#startDocument(int)}</td><td>content</td></tr>
 * <tr><td>open</td><td>{@link Outputter#startElement(NodeName, SchemaType, Location, int)}</td><td>content</td></tr>
 * <tr><td>content</td><td>{@link Outputter#characters(CharSequence, Location, int)}, {@link Outputter#comment(CharSequence, Location, int)},
 * {@link Outputter#processingInstruction(String, CharSequence, Location, int)}</td><td>content</td></tr>
 * <tr><td>content</td><td>{@link Outputter#startElement(NodeName, SchemaType, Location, int)}</td><td>startTag</td></tr>
 * <tr><td>content</td><td>{@link #endDocument()}, {@link #endElement()}</td><td>if the stack is empty, then content, otherwise open</td></tr>
 * <tr><td>(any)</td><td>close</td><td>final</td></tr>
 * <tr><td>final</td><td>close</td><td>final</td></tr>
 * </tbody>
 * </table>
 * <p>This class is not normally used in production within Saxon, but is available for diagnostics when needed.</p>
 * <p>Some implementations of {@code Receiver} accept sequences of events that are not regular; indeed, some
 * implementations are explicitly designed to produce a regular sequence from an irregular sequence.
 * Examples of such irregularities are <b>append</b> or <b>startDocument</b> events appearing within
 * element content, or <b>attribute</b> events being followed by <b>text</b> events with no intervening
 * <b>startContent</b>.</p>
 * <p>The rules for a <b>regular sequence</b> imply that the top level events (any events not surrounded
 * by startElement-endElement or startDocument-endDocument) can represent any sequence of items, including
 * for example multiple document nodes, free-standing attribute and namespace nodes, maps, arrays, and functions;
 * but within a startElement-endElement or startDocument-endDocument pair, the events represent content
 * that has been normalized and validated according to the XSLT rules for constructing complex content, or
 * the XQuery equivalent: for example, attributes and namespaces must appear before child nodes,
 * adjacent text nodes should
 * have been merged, zero-length text nodes should have been eliminated, all namespaces should be explicitly
 * declared, document nodes should be replaced by their children.</p>
 * <p>Element nodes in "composed form" (that is, existing as a tree in memory) may be passed through
 * the {@link #append(Item)} method at the top level, but within a startElement-endElement or
 * startDocument-endDocument pair, elements must be represented in "decomposed form" as a sequence
 * of events.</p>
 * <p>A call to {@link #close} is permitted in any state, but it should only be called in <code>Open</code>
 * state except on an error path; on error paths calling {@link #close} is recommended to ensure that
 * resources are released.</p>
 */
class RegularSequenceChecker(nextReceiver: Receiver,
                             private var fullChecking: Boolean)
  extends ProxyReceiver(nextReceiver) {

  private var stack: Stack[Short] = new Stack()

  private var state: State.State = State.Initial

  private def transition(event: Transition.Transition): Unit = {
    val map: Map[Transition.Transition, State.State] = machine.get(state)
    val newState: State.State = if (map == null) null else map.get(event)
    if (newState == null) {
      //assert false;
      throw new IllegalStateException(
        "Event " + event + " is not permitted in state " + state)
    } else {
      state = newState
    }
  }

  override def append(item: Item, locationId: Location, copyNamespaces: Int): Unit = {
    try {
      transition(Transition.APPEND)
      nextReceiver.append(item, locationId, copyNamespaces)
    } catch {
      case e: XPathException => {
        state = State.Failed
        throw e
      }

    }
  }

  override def characters(chars: CharSequence,
                          locationId: Location,
                          properties: Int): Unit = {
    transition(Transition.TEXT)
    if (chars.length == 0 && !stack.isEmpty) {
      throw new IllegalStateException(
        "Zero-length text nodes not allowed within document/element content")
    }
    try nextReceiver.characters(chars, locationId, properties)
    catch {
      case e: XPathException => {
        state = State.Failed
        throw e
      }

    }
  }

  override def close(): Unit = {
    if (state != State.Final && state != State.Failed) {
      if (!stack.isEmpty) {
        throw new IllegalStateException(
          "Unclosed element or document nodes at end of stream")
      }
      nextReceiver.close()
      state = State.Final
    }
  }

  override def comment(chars: CharSequence,
                       locationId: Location,
                       properties: Int): Unit = {
    transition(Transition.COMMENT)
    try nextReceiver.comment(chars, locationId, properties)
    catch {
      case e: XPathException => {
        state = State.Failed
        throw e
      }

    }
  }

  override def endDocument(): Unit = {
    transition(Transition.END_DOCUMENT)
    if (stack.isEmpty || stack.pop() != Type.DOCUMENT) {
      throw new IllegalStateException("Unmatched endDocument() call")
    }
    try nextReceiver.endDocument()
    catch {
      case e: XPathException => {
        state = State.Failed
        throw e
      }

    }
  }

  override def endElement(): Unit = {
    transition(Transition.END_ELEMENT)
    if (stack.isEmpty || stack.pop() != Type.ELEMENT) {
      throw new IllegalStateException("Unmatched endElement() call")
    }
    if (stack.isEmpty) {
      state = State.Open
    }
    try nextReceiver.endElement()
    catch {
      case e: XPathException => {
        state = State.Failed
        throw e
      }

    }
  }

  override def open(): Unit = {
    transition(Transition.OPEN)
    try nextReceiver.open()
    catch {
      case e: XPathException => {
        state = State.Failed
        throw e
      }

    }
  }

  override def processingInstruction(target: String,
                                     data: CharSequence,
                                     locationId: Location,
                                     properties: Int): Unit = {
    transition(Transition.PI)
    try nextReceiver.processingInstruction(target,
      data,
      locationId,
      properties)
    catch {
      case e: XPathException => {
        state = State.Failed
        throw e
      }

    }
  }

  override def startDocument(properties: Int): Unit = {
    transition(Transition.START_DOCUMENT)
    stack.push(Type.DOCUMENT)
    try nextReceiver.startDocument(properties)
    catch {
      case e: XPathException => {
        state = State.Failed
        throw e
      }

    }
  }

  override def startElement(elemName: NodeName,
                            `type`: SchemaType,
                            attributes: AttributeMap,
                            namespaces: NamespaceMap,
                            location: Location,
                            properties: Int): Unit = {
    transition(Transition.START_ELEMENT)
    stack.push(Type.ELEMENT)
    if (fullChecking) {
      attributes.verify()
      val prefix: String = elemName.getPrefix
      if (prefix.isEmpty) {
        val declaredDefaultUri: String = namespaces.getDefaultNamespace.toString
        if (declaredDefaultUri != elemName.getURI) {
          throw new IllegalStateException(
            "URI of element Q{" + elemName.getURI + "}" + elemName.getLocalPart +
              " does not match declared default namespace {" +
              declaredDefaultUri +
              "}")
        }
      } else {
        val declaredUri: String = namespaces.getURI(prefix)
        if (declaredUri == null) {
          throw new IllegalStateException(
            "Prefix " + prefix + " has not been declared")
        } else if (declaredUri != elemName.getURI) {
          throw new IllegalStateException(
            "Prefix " + prefix + " is bound to the wrong namespace")
        }
      }
      for (att <- attributes) {
        val name: NodeName = att.getNodeName
        if (!name.getURI.isEmpty) {
          val attPrefix: String = name.getPrefix
          val declaredUri: String = namespaces.getURI(attPrefix)
          if (declaredUri == null) {
            throw new IllegalStateException(
              "Prefix " + attPrefix + " has not been declared for attribute " +
                att.getNodeName.getDisplayName)
          } else if (declaredUri != name.getURI) {
            throw new IllegalStateException(
              "Prefix " + prefix + " is bound to the wrong namespace {" +
                declaredUri +
                "}")
          }
        }
      }
    }
    try nextReceiver.startElement(elemName,
      `type`,
      attributes,
      namespaces,
      location,
      properties)
    catch {
      case e: XPathException => {
        state = State.Failed
        throw e
      }

    }
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
