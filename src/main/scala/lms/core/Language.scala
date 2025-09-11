package lms.core

object Language {
  trait NodeT[K] {
    val kind: K

    def children: Vector[Int]
    def updateChildren(f: Int => Int): Unit

    def matches(other: NodeT[K]): Boolean

    def isLeaf: Boolean = children.isEmpty
  }
}

trait Language {
  type Kind
  type Node <: Language.NodeT[Kind]
}
