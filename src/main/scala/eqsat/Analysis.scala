package eqsat

import scala.collection.mutable

/**
 * An e-class analysis.
 *
 * @tparam K The discriminator kind of the language this analysis acts on
 * @tparam A The type of the analysis' results.
 */
trait Analysis[K, A] {
  /**
   * The analysis' unique identifier.
   */
  val id: String

  /**
   * Constructs a new analysis result for a newly created singleton e-class.
   * @param node The node in the singleton e-class.
   * @return An analysis result for the singleton e-class containing `node`.
   */
  def make(node: ENode[K]): A

  /**
   * Constructs a new analysis result for an e-node whose arguments have new results.
   * @param node The node in the whose arguments have changed.
   * @param result The old analysis result for the e-class.
   * @return A tentative analysis result for the e-class containing `node`, to be joined with the previous result for
   *         said e-class.
   */
  def reassemble(node: ENode[K], result: A): A = make(node)

  /**
   * When two e-classes are merged, join their analysis results into a new analysis result for the merged e-class.
   * @param left The analysis result of the first e-class being merged.
   * @param right The analysis result of the second e-class being merged.
   * @return A new analysis result for the merged e-class.
   */
  def join(left: A, right: A): A

  /**
   * Optionally modify an e-class based on its analysis result. Calling `modify` on the same e-class more than once
   * must produce the same result as calling it only once.
   * @param graph The graph that defines `eClass`.
   * @param eClass The e-class to potentially modify.
   * @param analysisResult This analysis' result for `eClass`.
   */
  def modify(graph: EGraph[K])(eClass: EClass[K], analysisResult: A): Unit

  /**
   * Tests if two analysis results are equivalent.
   * @param left A first analysis result.
   * @param right A second analysis result.
   * @return `true` if the analysis results are equivalent; otherwise, `false`.
   */
  def areEquivalent(left: A, right: A): Boolean = left == right

  /**
   * Asserts that this analysis' results are consistent when recomputed on a graph.
   * @param graph The e-graph to check.
   */
  final def assertConsistency(graph: EGraph[K]): Unit = {
    val oldResults = graph.classes.map(c => (c, c.analysisResult(this))).toMap
    graph.recomputeAnalysis(this)
    for (c <- graph.classes) {
      if (!areEquivalent(c.analysisResult(this), oldResults(c))) {
        println(s"Inconsistent result for $id on $c: ${oldResults(c)} vs ${c.analysisResult(this)}")
        println(c.nodes.map(n => (n, n.hashCode())))
        println(c.nodes.flatMap(_.args).distinct.map(x => (x, x.analysisResult(this))))
        println(c.nodes.flatMap(_.args).distinct.map(x => (x, oldResults(x.asInstanceOf[EClass[K]]))))
        assert(areEquivalent(c.analysisResult(this), oldResults(c)))
      }
    }
  }
}

/**
 * A fixed collection of e-class analyses, suitable for inclusion in an e-graph.
 * @param analyses A set of e-class analyses.
 */
case class Analyses[K](analyses: Seq[Analysis[K, Any]])

/**
 * Constructs `Analyses` instances.
 */
class AnalysesBuilder[K] {
  private val analyses = new mutable.ArrayBuffer[Analysis[K, Any]]()

  /**
   * Turns this builder into an `Analyses` instance.
   * @return An `Analyses` instance.
   */
  def toAnalyses: Analyses[K] = Analyses[K](analyses.clone().toSeq)

  /**
   * Adds an analysis to this builder's set of analyses.
   * @param analysis The analysis to add to this builder.
   * @tparam A The type of analysis results produced by `analysis`.
   * @return This instance.
   */
  def add[A](analysis: Analysis[K, A]): AnalysesBuilder[K] = {
    analyses.append(ErasedAnalysis[A](analysis))
    this
  }

  private case class ErasedAnalysis[A](analysis: Analysis[K, A]) extends Analysis[K, Any] {
    /**
     * The analysis' unique identifier.
     */
    override val id: String = analysis.id

    /**
     * When two e-classes are merged, join their analysis results into a new analysis result for the merged e-class.
     *
     * @param left  The analysis result of the first e-class being merged.
     * @param right The analysis result of the second e-class being merged.
     * @return A new analysis result for the merged e-class.
     */
    override def join(left: Any, right: Any): Any = analysis.join(left.asInstanceOf[A], right.asInstanceOf[A])

    /**
     * Optionally modify an e-class based on its analysis result. Calling `modify` on the same e-class more than once
     * must produce the same result as calling it only once.
     *
     * @param graph          The graph that defines `eClass`.
     * @param eClass         The e-class to potentially modify.
     * @param analysisResult This analysis' result for `eClass`.
     */
    override def modify(graph: EGraph[K])(eClass: EClass[K], analysisResult: Any): Unit =
      analysis.modify(graph)(eClass, analysisResult.asInstanceOf[A])

    /**
     * Constructs a new analysis result for a newly created singleton e-class.
     *
     * @param node  The node in the singleton e-class.
     * @return An analysis result for the singleton e-class containing `node`.
     */
    override def make(node: ENode[K]): Any = analysis.make(node)

    /**
     * Constructs a new analysis result for an e-node whose arguments have new results.
     * @param node The node in the whose arguments have changed.
     * @param result The old analysis result for the e-class.
     * @return A tentative analysis result for the e-class containing `node`, to be joined with the previous result for
     *         said e-class.
     */
    override def reassemble(node: ENode[K], result: Any): Any =
      analysis.reassemble(node, result.asInstanceOf[A])

    override def areEquivalent(left: Any, right: Any): Boolean =
      analysis.areEquivalent(left.asInstanceOf[A], right.asInstanceOf[A])
  }
}
