package lms.core

import scala.collection.mutable.{Buffer, ArrayBuffer, Map, HashMap, Stack}

class EGraph(val lang: Language, val analysis: Analysis) {
  private var nodes: Buffer[lang.Node] = ArrayBuffer.empty[lang.Node]
  private var unionfind: UnionFind = new UnionFind
  private var analysisPending: Stack[Int] = Stack.empty[Int]
  private var pending: Stack[Int] = Stack.empty[Int]
  private var eclasses: Map[Int, EClass[lang.Kind, lang.Node]] = HashMap.empty
  // XXX: do we need this?
  private var memo: Map[lang.Node, Int] = HashMap.empty[lang.Node, Int]

  private var clean: Boolean = true

  private def mkEClass(node: lang.Node): Int = {
    val id = unionfind.fresh()
    val cls = EClass(id, ArrayBuffer(node), (), ArrayBuffer())

    nodes.append(node)
    pending.push(id)
    memo(node) = id

    id
  }

  private def lookupInternal(node: lang.Node): Option[Int] = {
    canonicalize(node)
    memo.get(node)
  }

  private def find(id: Int): Int = {
    unionfind.find(id)
  }

  // XXX - should this be moved to `Node`?
  private def canonicalize(node: lang.Node): Unit = {
    node.updateChildren(unionfind.find)
  }

  def addUncanonical(node: lang.Node): Int = {
    lookupInternal(node) match {
      case Some(existing) => existing
      case None => {
        val id = mkEClass(node)
        //analysis.modify(this, id)
        this.clean = false
        id
      }
    }
  }

  private def performUnion(nid1: Int, nid2: Int): Boolean = {
    //analysis.preUnion(id1, id2)

    this.clean = false

    val id1 = find(nid1)
    val id2 = find(nid2)

    if (id1 == id2) {
      return false
    }

    val nparents1 = eclasses(id1).parents.size
    val nparents2 = eclasses(id2).parents.size

    // this is incredibly annoying and error-prone.
    val (vid1, vid2) = if (nparents1 >= nparents2) then (id1, id2) else (id2, id1)

    unionfind.union(vid1, vid2)

    val cls2 = eclasses.remove(vid2) match {
      case None => throw new RuntimeException("BUG: `eclasses.remove(vid2)` returned None")
      case Some(cls) => cls
    }
    val cls1 = eclasses(vid1)

    pending ++= cls2.parents

    /*
    val (merge1, merge2) = analysis.merge(cls1.data, cls2.data)
    if (merge1) {
      analysisPending ++= cls1.parents
    }
    if (merge2) {
      analysisPending ++= cls2.parents
    }
    */

    cls1.nodes ++= cls2.nodes
    cls1.parents ++= cls2.parents

    //analysis.modify(vid1)
    true
  }

  private def processUnions(): Int = {
    var result = 0

    while (!pending.isEmpty || !analysisPending.isEmpty) {
      for (cls <- pending.popAll()) {
        val node = nodes(cls)
        canonicalize(node)
        memo.put(node, cls) match {
          case Some(memoCls) => {
            if (performUnion(memoCls, cls)) {
              result += 1
            }
          }
          case None => {}
        }
      }
      while (!analysisPending.isEmpty) {
      }
    }

    result
  }

  private def rebuildClasses(): Unit = {}

  def rebuild(): Int = {
    val oldHCSize = memo.size
    val oldNumEClasses = eclasses.size

    val numUnions = processUnions()
    rebuildClasses()

    this.clean = true
    numUnions
  }
}
