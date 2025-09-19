package lms.eqsat

import scala.collection.mutable

import lms.util.Plumbing._
import lms.eqsat.Syntax._

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
        makePass(cls) match {
          case None => {}
          case Some(nnew) => {
            didSomething = true
            costs.updateWith(cls) {
              case None => Some(nnew)
              case Some(nold) => Some(Ordering.by[(Cost, ENode[K]), Cost](_._1).min(nold, nnew))
            }
          }
        }
      }
    }
    this.populated = true
  }

  final def extract(root: EClass[K]): FlatTree[K] = {
    if (!this.populated) {
      this.populate()
    }

    val cache: mutable.Map[EClass[K], Int] = mutable.Map.empty
    val nodes: mutable.ArrayBuffer[FlatNode[K]] = mutable.ArrayBuffer.empty

    def walk(cls: EClass[K]): Int = {
      cache.getOrElse(cls, {
        val (_, node) = costs(cls)
        val idx = nodes.length
        nodes += FlatNode(node.kind, node.children.map(walk))
        cache(cls) = idx
        idx
      })
    }

    val _ = walk(root)
    FlatTree(nodes.toVector)
  }
}
