package eqsat

import scala.collection.mutable

class EGraph[K](
  val analyses: Analyses[K],
  val rebuildAfterMerge: Boolean = false,
  val shouldPrintProgress: Boolean = false,
  val checkConsistency: Boolean = false,
  val deferMerges: Boolean = false,
) {
  def hasAnalysis[A](analysis: Analysis[K, A]): Boolean = analyses.analyses.exists(_.id == analysis.id)

  val unionFind = new DisjointSetForest[EClass[K]]()
  val hashCons = new mutable.HashMap[ENode[K], EClass[K]]()

  private val mergeBuffer = mutable.ArrayBuffer[(EClass[K], EClass[K])]()
  private val repairWorklist = mutable.ArrayBuffer[EClass[K]]()

  def classes: Seq[EClass[K]] = hashCons.values.toSeq.map(find).distinct.sorted

  def canonicalize(node: ENode[K]): ENode[K] = node.mapArgs(find)

  def add(node: ENode[K]): EClass[K] = {
    val canonicalNode = canonicalize(node)
    hashCons.get(canonicalNode) match {
      case Some(existing) => existing
      case None => {
        val cls = makeNewEClass(canonicalNode)
        hashCons.put(canonicalNode, cls)
        cls
      }
    }
  }

  private final def makeNewEClass(node: ENode[K]): EClass[K] = {
    // Create an e-class.
    val cls = EClass[K](this)

    // Add the node to the e-class.
    cls.nodeSet.add(node)

    // Make the node's arguments point to this e-class.
    for (child <- node.args) {
      child.parents.append((node, cls))
    }

    // Register the e-class with the union-find data structure.
    val id = unionFind.find(cls)

    // Set up the e-class' analysis results.
    for (analysis <- analyses.analyses) {
      val result = analysis.make(node)
      cls.setAnalysisResult(analysis, result)
      analysis.modify(this)(cls, result)
    }

    id
  }

  def merge(left: EClass[K], right: EClass[K]): Boolean = {
    if (deferMerges) {
      mergeBuffer.append((left, right))
      find(left) != find(right)
    }
    else {
      val result = mergeImpl(left, right)
      if (rebuildAfterMerge) then rebuild()
      result
    }
  }

  def mergeImpl(left: EClass[K], right: EClass[K]): Boolean = {
    // Find the set leader for the left and right classes.
    val (leftRoot, rightRoot) = (unionFind.find(left), unionFind.find(right))
    if (leftRoot == rightRoot) {
      return false
    }

    val leftAnalysisResults = analyses.analyses.map(a => (a, leftRoot.analysisResult(a))).toMap
    val rightAnalysisResults = analyses.analyses.map(a => (a, rightRoot.analysisResult(a))).toMap

    // Unify the left and right leaders.
    val newId = unionFind.union(leftRoot, rightRoot)

    // Construct a list of e-node, e-class pairs that need to be canonicalized and checked for congruence.
    repairWorklist.append(newId)

    val (oldId, oldAnalysisResults, newAnalysisResults) = if (newId == leftRoot) {
      (rightRoot, rightAnalysisResults, leftAnalysisResults)
    } else {
      (leftRoot, leftAnalysisResults, rightAnalysisResults)
    }

    newId.parents ++= oldId.parents
    for (node <- oldId.nodeSet) {
      newId.nodeSet.add(node)

      // Patch up parent lists of child classes.
      for (arg <- node.args) {
        repairWorklist.append(arg)
      }
    }

    // Patch up parent lists of sibling classes.
    for ((_, parent) <- oldId.parents) {
      for (node <- parent.nodes) {
        for (arg <- node.args) {
          if (find(arg) != newId) {
            repairWorklist.append(arg)
          }
        }
      }
    }

    // Update the merged e-class' analysis results.
    for (analysis <- analyses.analyses) {
      val newResult = analysis.join(newAnalysisResults(analysis), oldAnalysisResults(analysis))
      newId.setAnalysisResult(analysis, newResult)
    }

    true
  }

  def find(cls: EClass[K]): EClass[K] = unionFind.find(cls)

  /**
   * Computes an analysis' results for the entire graph.
   * @param analysis The analysis to compute.
   * @tparam A The type of the analysis' results.
   */
  def recomputeAnalysis[A](analysis: Analysis[K,A]): Unit = {
    // Clear the analysis results.
    for (eClass <- classes) {
      eClass.clearAnalysisResult(analysis)
    }

    // Compute analyses until we reach a fixpoint.
    var worklist = mutable.ArrayBuffer[EClass[K]]()
    worklist ++= classes
    while (worklist.nonEmpty) {
      val newWorklist = mutable.ArrayBuffer[EClass[K]]()
      for (eClass <- classes) {
        val eligibleNodes = eClass.nodes.filter(_.args.forall(_.hasAnalysisResult(analysis)))
        val nodeResults = eligibleNodes.map(analysis.make)
        if (nodeResults.nonEmpty) {
          var classResult = nodeResults.reduce(analysis.join)
          val hasChanged = if (eClass.hasAnalysisResult(analysis)) {
            val oldResult = eClass.analysisResult(analysis)
            classResult = analysis.join(oldResult, classResult)
            classResult != oldResult
          } else {
            true
          }

          if (hasChanged) {
            eClass.setAnalysisResult(analysis, classResult)
            newWorklist ++= eClass.parents.map(_._2).map(find)
          }
        }
      }
      worklist = newWorklist
    }

    // Check that we computed analyses for all e-classes.
    for (eClass <- classes) {
      assert(eClass.hasAnalysisResult(analysis))
    }
  }

  def rebuild(): Unit = {
    for ((left, right) <- mergeBuffer) {
      mergeImpl(left, right)
    }
    mergeBuffer.clear()

    if (checkConsistency && shouldPrintProgress) then println("=== rebuilding ===")
    while (repairWorklist.nonEmpty) {
      val todo = repairWorklist.map(find).distinct
      repairWorklist.clear()
      for (eClass <- todo) {
        repair(find(eClass))
      }
    }
    if (checkConsistency) {
      if (shouldPrintProgress) then println("=== done ===")
      assertConsistency()
      for (analysis <- analyses.analyses) {
        analysis.assertConsistency(this)
      }
      if (shouldPrintProgress) then println("=== consistent ===")
    }
  }

  private def repair(eClass: EClass[K]): Unit = {
    repairHashcons(eClass)

    for (analysis <- analyses.analyses) {
      analysis.modify(this)(find(eClass), find(eClass).analysisResult(analysis))

      for ((parentNode, parentClass) <- find(eClass).parents) {
        val canonicalNode = canonicalize(parentNode)
        val canonicalClass = find(parentClass)
        val oldResult = canonicalClass.analysisResult(analysis)
        val newResult = analysis.join(oldResult, analysis.make(canonicalNode))
        if (newResult != oldResult) {
          canonicalClass.setAnalysisResult(analysis, newResult)
          repairWorklist.append(canonicalClass)
        }
      }
    }
  }

  private def repairHashcons(eClass: EClass[K]): Unit = {
    for ((parentNode, parentClass) <- eClass.parents) {
      val canonicalNode = canonicalize(parentNode)
      val canonicalClass = find(parentClass)
      hashCons.remove(parentNode)
      canonicalClass.nodeSet.remove(parentNode)
      hashCons(canonicalNode) = parentClass
      canonicalClass.nodeSet.add(canonicalNode)
    }

    repairParents(eClass)
  }

  private def repairParents(eClass: EClass[K]): Unit = {
    val classesToMerge = mutable.Buffer[(EClass[K], EClass[K])]()
    val newParents = mutable.Map[ENode[K], EClass[K]]()
    for ((parentNode, parentClass) <- eClass.parents) {
      val canonicalNode = canonicalize(parentNode)
      newParents.get(canonicalNode) match {
        case Some(otherClass) if otherClass != parentClass =>
          //          println(s"Upward merging $parentClass and $otherClass")
          classesToMerge.append((parentClass, otherClass))

        case _ =>
      }
      newParents(canonicalNode) = parentClass
    }

    find(eClass).setParents(newParents.toSeq)

    for ((left, right) <- classesToMerge) {
      mergeImpl(left, right)
    }
  }

  final def assertConsistency(): Unit = {
    assertNoDanglingClasses()
    for (eClass <- classes) {
      // Check that the parents list is consistent.
      for ((parentNode, parentClass) <- eClass.parents) {
        assert(parentNode == canonicalize(parentNode))
        assert(find(hashCons(parentNode)) == find(parentClass))
        assert(parentNode.args.contains(eClass))
      }

      // Check that the nodes set is consistent.
      for (node <- eClass.nodeSet) {
        assert(find(hashCons(node)) == eClass)
        for (arg <- node.args) {
          assert(arg.parents.map(t => (t._1, find(t._2))).contains((node, eClass)))
        }
      }
    }
  }

  final protected def assertNoDanglingClasses(): Unit = {
    // Hashcons values may be stale (i.e., non-canonical) but the keys must be canonical at all times.
    for (eNode <- hashCons.keys) {
      for (arg <- eNode.args) {
        if (arg.stale) {
          println(s"Stale node $eNode with canonical variant ${canonicalize(eNode)} (in class ${hashCons(eNode)})")
          println(s"$arg parents: ${arg.parents}")
          println(s"${find(arg)} parents: ${find(arg).parents}")
        }
        assert(!arg.stale)
      }
    }
  }

  final def saturate
}

case class ENode[K](
  val kind: K,
  val args: Seq[EClass[K]],
) {
  def mapArgs(f: EClass[K] => EClass[K]): ENode[K] = {
    ENode[K](kind, args.map(f))
  }
}

class EClass[K] private(val owner: EGraph[K]) extends Ordered[EClass[K]] {
  val id = EClass.freshId()

  override def hashCode(): Int = id.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case e: EClass[_] => id == e.id
    case _ => false
  }

  def compare(other: EClass[K]): Int = id.compare(other.id)

  val parents: mutable.Buffer[(ENode[K], EClass[K])] = new mutable.ArrayBuffer()

  val nodeSet = new mutable.HashSet[ENode[K]]()

  def stale: Boolean = owner.find(this) != this

  private val analysisResults: mutable.HashMap[String, Any] = mutable.HashMap()

  def analysisResult[A](analysis: Analysis[K, A]): A =
    owner.find(this).analysisResults(analysis.id).asInstanceOf[A]

  def setAnalysisResult[A](analysis: Analysis[K, A], result: A): Unit =
    owner.find(this).analysisResults(analysis.id) = result

  def clearAnalysisResult[A](analysis: Analysis[K, A]): Option[Any] =
    owner.find(this).analysisResults.remove(analysis.id)

  def hasAnalysisResult[A](analysis: Analysis[K, A]): Boolean =
    owner.find(this).analysisResults.contains(analysis.id)

  def nodes: Seq[ENode[K]] = owner.find(this).nodeSet.toSeq

  def setParents(newParents: Seq[(ENode[K], EClass[K])]): Unit = {
    parents.clear()
    parents ++= newParents
  }
}

object EClass {
  private var ctr = 0

  def freshId(): Int = {
    val result = ctr
    ctr += 1
    result
  }

  def apply[K](graph: EGraph[K]): EClass[K] = new EClass(graph)
}
