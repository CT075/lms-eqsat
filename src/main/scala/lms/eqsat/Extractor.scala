package lms.eqsat

import scala.collection.mutable

abstract class Extractor[K](graph: EGraph[K]) {
  type Cost

  val costComparator: Ordering[Cost]

  def nodeCost(kind: K, childCosts: Seq[Cost]): Cost

  private val costs: mutable.Map[EClass[K], Cost] = mutable.Map.empty

  private def totalCost(node: ENode[K]): Option[Cost] = _
}
