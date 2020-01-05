package de.srtobi.dfa
package cfg

abstract class JumpingInstruction extends Instruction {
  def targetLabel: Label
}
