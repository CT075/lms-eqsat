package lms.eqsat

object Syntax {
  case class RecNode[K](kind: K, children: Seq[RecNode[K]])

  // Invariant: A node's children ids must be strictly less than that of the
  // node itself
  case class FlatNode[K](kind: K, children: Seq[Int])
  case class FlatTree[K](nodes: Seq[FlatNode[K]]) {
    def toRecNode: RecNode[K] = {
      val cache: Array[RecNode[K]] = new Array(this.nodes.length)

      this.nodes.zipWithIndex.foreach { (node, i) =>
        cache(i) = RecNode(node.kind, node.children.map(cache(_)))
      }

      cache(this.nodes.length-1)
    }
  }
}
