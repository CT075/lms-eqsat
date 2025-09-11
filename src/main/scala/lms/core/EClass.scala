package lms.core

import scala.collection.mutable.Buffer

// TODO: Maybe we can make `lang` implicit.
class EClass[K, L <: Language.NodeT[K]](
  val id: Int,
  val nodes: Buffer[L],
  val data: Unit,
  val parents: Buffer[Int]
) {
  def leaves: Vector[L] = nodes.filter(_.isLeaf).toVector
}
