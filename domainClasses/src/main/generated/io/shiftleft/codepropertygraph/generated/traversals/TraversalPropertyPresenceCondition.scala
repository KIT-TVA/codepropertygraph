package io.shiftleft.codepropertygraph.generated.traversals

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.codepropertygraph.generated.accessors.languagebootstrap.*

final class TraversalPropertyPresenceCondition[
  NodeType <: nodes.StoredNode & nodes.StaticType[nodes.HasPresenceConditionEMT]
](val traversal: Iterator[NodeType])
    extends AnyVal {

  /** Traverse to presenceCondition property */
  def presenceCondition: Iterator[String] =
    traversal.map(_.presenceCondition)

  /** Traverse to nodes where the presenceCondition matches the regular expression `value`
    */
  def presenceCondition(pattern: String): Iterator[NodeType] =
    if (!flatgraph.misc.Regex.isRegex(pattern)) {
      presenceConditionExact(pattern)
    } else {
      val matcher = flatgraph.misc.Regex.multilineMatcher(pattern)
      traversal.filter { item => matcher.reset(item.presenceCondition).matches }
    }

  /** Traverse to nodes where the presenceCondition matches at least one of the regular expressions in `values`
    */
  def presenceCondition(patterns: String*): Iterator[NodeType] = {
    val matchers = patterns.map(flatgraph.misc.Regex.multilineMatcher)
    traversal.filter { item => matchers.exists { _.reset(item.presenceCondition).matches } }
  }

  /** Traverse to nodes where presenceCondition matches `value` exactly.
    */
  def presenceConditionExact(value: String): Iterator[NodeType] = traversal match {
    case init: flatgraph.misc.InitNodeIterator[flatgraph.GNode @unchecked] if init.isVirgin && init.hasNext =>
      val someNode = init.next
      flatgraph.Accessors
        .getWithInverseIndex(someNode.graph, someNode.nodeKind, 44, value)
        .asInstanceOf[Iterator[NodeType]]
    case _ => traversal.filter { _.presenceCondition == value }
  }

  /** Traverse to nodes where presenceCondition matches one of the elements in `values` exactly.
    */
  def presenceConditionExact(values: String*): Iterator[NodeType] = {
    if (values.length == 1) return presenceConditionExact(values.head)
    traversal match {
      case init: flatgraph.misc.InitNodeIterator[flatgraph.GNode @unchecked] if init.isVirgin && init.hasNext =>
        val someNode = init.next
        values.iterator.flatMap { value =>
          flatgraph.Accessors
            .getWithInverseIndex(someNode.graph, someNode.nodeKind, 44, value)
            .asInstanceOf[Iterator[NodeType]]
        }
      case _ =>
        val valueSet = values.toSet
        traversal.filter { item => valueSet.contains(item.presenceCondition) }
    }
  }

  /** Traverse to nodes where presenceCondition does not match the regular expression `value`.
    */
  def presenceConditionNot(pattern: String): Iterator[NodeType] = {
    if (!flatgraph.misc.Regex.isRegex(pattern)) {
      traversal.filter { node => node.presenceCondition != pattern }
    } else {
      val matcher = flatgraph.misc.Regex.multilineMatcher(pattern)
      traversal.filterNot { item => matcher.reset(item.presenceCondition).matches }
    }
  }

  /** Traverse to nodes where presenceCondition does not match any of the regular expressions in `values`.
    */
  def presenceConditionNot(patterns: String*): Iterator[NodeType] = {
    val matchers = patterns.map(flatgraph.misc.Regex.multilineMatcher)
    traversal.filter { item => matchers.find { _.reset(item.presenceCondition).matches }.isEmpty }
  }

}
