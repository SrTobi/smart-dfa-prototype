package de.srtobi.dfaTest
package dfa
package impl2

import de.srtobi.dfaTest.cfg.Debug
import de.srtobi.dfaTest.dfa.impl2.GatherState.Global

import scala.collection.mutable

class GatherState(val global: Global,
                  private val blockStack: List[Block],
                  private var registers: Map[DfRegister, Set[Value]],
                  private var properties: Map[String, Set[WritePropertyOperation]]) {

  def newRegister(register: DfRegister, value: Value): Value = {
    assert(!registers.contains(register))
    registers += (register -> Set(value))
    value
  }

  def register(variable: DfRegister): Value = {
    val set = registers(variable)

    if (set.size == 1) set.head
    else {
      val unified = unify(set)
      registers += (variable -> Set(unified))
      unified
    }
  }

  def unknownValue(name: String): Value =
    newOperation(new UnknownValue(name))

  def unify(set: Set[Value]): Value =
    newOperation(new UnifyOperation(set))

  def constant(value: DfAbstractAny): Value =
    newOperation(new Constant(value))

  def makeEquality(left: Value, right: Value): Value =
    newOperation(new EqualityOperation(left, right))

  private def propsFor(prop: String): Set[WritePropertyOperation] =
    properties.getOrElse(prop, Set.empty)

  def readProp(base: Value, prop: String): Value = {
    newOperation(new ReadPropertyOperation(base, prop, propsFor(prop)))
  }

  def writeProp(base: Value, prop: String, value: Value): Value = {
    val op = newOperation(new WritePropertyOperation(base, prop, value, propsFor(prop)))
    properties += prop -> Set(op)
    value
  }

  def report(report: Debug.Report): Unit = {
    global.reports += report -> newOperation(new Summary(Seq.empty, properties, global.localScopeObj))
  }

  def split(condition: Value): (GatherState, GatherState) = {
    condition.isBlockCondition = true
    (
      new GatherState(global, Block(condition, targetTruthValue = true) :: blockStack, registers, properties),
      new GatherState(global, Block(condition, targetTruthValue = false) :: blockStack, registers, properties)
    )
  }

  private def newOperation[T <: Operation](op: T): T = {
    op.block = blockStack.headOption
    global.operations += op
    op
  }
}

case class Block(condition: Value, targetTruthValue: Boolean)


object GatherState {
  def empty = new GatherState(new Global, Nil, Map.empty, Map.empty)

  class Global {
    //var allVariables: List[Value] = List.empty
    val localScopeObj = new DfConcreteObjectRef("localScope")
    val localScope = new Constant(localScopeObj)
    val reports = mutable.Map.empty[Debug.Report, Summary]
    val operations: mutable.Buffer[Operation] = mutable.Buffer(localScope)
  }


  def unify(states: Iterable[GatherState]): GatherState = {
    val minBlockStack = states.iterator.map(_.blockStack.length).min

    assert(
      states.iterator
        .map(_.blockStack.takeRight(minBlockStack))
        .sliding(2)
        .withPartial(false)
        .forall { case Seq(a, b) => a == b }
    )

    val registers = states.iterator
      .map(_.registers)
      .reduce {
        (a, b) => a.mergeWith(b) {
          (r1, r2) => r1 | r2
        }
      }


    val properties = states.iterator
      .map(_.properties)
      .reduce {
        (a, b) => a.mergeWith(b) {
          (r1, r2) => r1 | r2
        }
      }


    val head = states.head
    new GatherState(head.global, head.blockStack.takeRight(minBlockStack), registers, properties)
  }
}