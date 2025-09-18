package lms.eqsat

import scala.collection.mutable

import lms.util.Plumbing._

abstract class Extractor[K](graph: EGraph[K]) {
  type Cost

  implicit val costOrd: Ordering[Cost]

  def cost(kind: K, childCosts: Seq[Cost]): Cost

  private var populated = false
  private val costs: mutable.Map[EClass[K], (Cost, ENode[K])] = mutable.Map.empty

  private def totalCost(node: ENode[K]): Option[Cost] =
    node.children.map(costs.get(_)).sequence.map(c => cost(node.kind, c.map(_._1)))

  private def makePass(cls: EClass[K]): Option[(Cost, ENode[K])] = {
    given mcostOrd: Ordering[Option[Cost]] = new Ordering[Option[Cost]] {
      def compare(l: Option[Cost], r: Option[Cost]): Int = (l, r) match {
        case (None, None) => 0
        case (None, _) => -1
        case (_, None) => 1
        case (Some(x), Some(y)) => costOrd.compare(x,y)
      }
    }
    val (cost, node) = cls.nodeSet.map(n => (totalCost(n), n)).minBy(_._1)
    cost.map((_, node))
  }

  private def populate(): Unit = {
    var didSomething = true
    while (didSomething) {
      didSomething = false

      graph.classes.foreach { cls =>
        (costs.get(cls), makePass(cls)) match {
          case (None, Some(n)) => {
            costs(cls) = n
            didSomething = true
          }
          case (Some(nold), Some(nnew)) => {
            costs(cls) = Ordering.by[(Cost, ENode[K]), Cost](_._1).min(nold, nnew)
            didSomething = true
          }
          case _ => {}
        }
      }
    }
    this.populated = true
  }

  final def extract(cls: EClass[K]): Option[Unit] = {
    if (!this.populated) {
      this.populate()
    }

    assert(this.populated)
  }
}
