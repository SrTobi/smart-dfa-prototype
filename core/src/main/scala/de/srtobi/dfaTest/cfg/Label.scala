package de.srtobi.dfaTest
package cfg

abstract class Label {
  def name: String
  def nameWithoutLine: String
  def line: Int = targetIndex + 1
  def targetIndex: Int
  def target: cfg.Instruction = graph.instructionAt(targetIndex)
  def graph: ControlFlowGraph

  override def toString: String = ".L" + name
  def toStringWithoutLine: String = ".L" + nameWithoutLine
}
