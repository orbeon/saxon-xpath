package net.sf.saxon.om

import java.util.{HashMap, HashSet, Map, Set}

import scala.jdk.CollectionConverters._

class DocumentPool {

  private val documentNameMap: Map[DocumentURI, TreeInfo] =
    new HashMap[DocumentURI, TreeInfo](10)

  private val unavailableDocuments: Set[DocumentURI] =
    new HashSet[DocumentURI](10)

  def add(doc: TreeInfo, uri: String): Unit =
    synchronized {
      if (uri != null)
        documentNameMap.put(new DocumentURI(uri), doc)
    }

  def add(doc: TreeInfo, uri: DocumentURI): Unit =
    synchronized {
      if (uri != null) {
        documentNameMap.put(uri, doc)
      }
    }

  def find(uri: String): TreeInfo = synchronized {
    documentNameMap.get(new DocumentURI(uri))
  }

  def find(uri: DocumentURI): TreeInfo = synchronized {
    documentNameMap.get(uri)
  }

  def getDocumentURI(doc: NodeInfo): String = synchronized {
    for (uri <- documentNameMap.keySet.asScala) {
      val found = find(uri)
      if (found == null) {
        // continue; // can happen when discard-document() is used concurrently
      } else if (found.getRootNode == doc) {
        return uri.toString
      }
    }
    null
  }

  def contains(doc: TreeInfo): Boolean = synchronized {
    documentNameMap.values.contains(doc)
  }

  def discard(doc: TreeInfo): TreeInfo = synchronized {
    for ((key, value) <- documentNameMap.asScala) {
      val name: DocumentURI = key
      val entry: TreeInfo = value
      if (entry == doc) {
        documentNameMap.remove(name)
        return doc
      }
    }
    doc
  }

  /*def discardIndexes(keyManager: KeyManager): Unit = { // KeyManager not exist
    for (doc <- documentNameMap.values.asScala) {
      keyManager.clearDocumentIndexes(doc)
   }
   }*/

  def markUnavailable(uri: DocumentURI): Unit =
    unavailableDocuments.add(uri)

  def isMarkedUnavailable(uri: DocumentURI): Boolean =
    unavailableDocuments.contains(uri)
}
