package lms.eqsat

import scala.collection.mutable

/**
 * A disjoint set forest data structure.
 * @tparam A The element type of the disjoint set forest.
 */
final class DisjointSetForest[A] {
  private class NodeInfo(val element: A, var parent: NodeInfo, var size: Int) {
    def setParent(other: NodeInfo): Unit = {
      if (this != other) {
        parent = other
      }
    }
  }

  private val elementsToNodes: mutable.HashMap[A, NodeInfo] = new mutable.HashMap[A, NodeInfo]()

  /**
   * Finds the representative of the set of items to which `element` is equivalent.
   * @param element A node to query.
   * @return The representative of the set of items to which `element` is equivalent.
   */
  def find(element: A): A = {
    findRootImpl(makeSetImpl(element)).element
  }

  def union(x: A, y: A): A = {
    unionImpl(makeSetImpl(x), makeSetImpl(y)).element
  }

  private def findRootImpl(element: NodeInfo): NodeInfo = element.parent match {
    case node if node == element => node
    case _ =>
      element.setParent(findRootImpl(element.parent))
      element.parent
  }

  private def makeSetImpl(element: A): NodeInfo = {
    elementsToNodes.getOrElseUpdate(element, {
      val node = new NodeInfo(element,null, 1)
      node.parent = node
      node
    })
  }

  private def unionImpl(x: NodeInfo, y: NodeInfo): NodeInfo = {
    var root_x = findRootImpl(x)
    var root_y = findRootImpl(y)

    if (root_x == root_y) {
      // The sets have already been merged.
      return root_x
    }

    // Make sure x has at least as many descendants as y.
    if (root_x.size < root_y.size) {
      val tmp = root_x
      root_x = root_y
      root_y = tmp
    }

    // Make x the new root.
    root_y.setParent(root_x)

    // Update x's size.
    root_x.size += root_y.size
    root_x
  }
}
