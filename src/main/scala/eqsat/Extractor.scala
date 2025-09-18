package eqsat

import scala.collection.mutable

abstract class Extractor[K](graph: EGraph[K]) {
  type Cost

  val costComparator: Ordering[Cost]

  val costs: mutable.Map[EClass[K], Cost] = mutable.Map.empty

  private def totalCost(node: ENode[K]): Option[Cost] = {
    node.children.foldLeft
  }
}
