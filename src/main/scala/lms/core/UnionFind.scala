package lms.core

import scala.collection.mutable.{Buffer, ArrayBuffer}

class UnionFind {
  private var parents: Buffer[Int] = ArrayBuffer.empty[Int]

  def fresh(): Int = {
    val id = parents.size
    parents ++= List(id)
    id
  }

  def size: Int = parents.size

  private def parent(i: Int): Int = parents(i)

  def find(x: Int): Int = {
    var current = x
    while (true) {
      val p = parent(current)
      if (p == current) then return current

      val gp = parent(p)
      parents(current) = gp
      current = gp
    }
    current
  }

  def union(root1: Int, root2: Int): Int = {
    parents(root2) = root1
    root1
  }
}
