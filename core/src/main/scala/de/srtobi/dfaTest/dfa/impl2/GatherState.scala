package de.srtobi.dfaTest
package dfa
package impl2

import de.srtobi.dfaTest.cfg.Debug
import de.srtobi.dfaTest.dfa.impl2.GatherState.Global

import scala.collection.mutable

class GatherState(val global: Global,
                  private val blockStack: List[Block],
                  //private val conditions: Map[Value, Boolean],
                  private var registers: Map[DfRegister, Set[Value]],
                  private var properties: Map[String, PropertyMemorySource]) {

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

  def truthify(value: Value): Value = {
    newOperation(new TruthyOperation(value))
  }

  def unknownValue(name: String): Value =
    newOperation(new UnknownValue(name))

  def unify(set: Set[Value]): Value =
    newOperation(new UnifyOperation(set))

  def constant(value: DfAbstractAny): Value =
    newOperation(new Constant(value))

  def makeEquality(left: Value, right: Value): Value =
    newOperation(new EqualityOperation(left, right))

  private def propsFor(prop: String): PropertyMemorySource =
    properties.getOrElse(prop, global.propertyInit)

  def readProp(base: Value, prop: String): Value = {
    newOperation(new ReadPropertyOperation(base, prop, propsFor(prop)))
  }

  def writeProp(base: Value, prop: String, value: Value): Value = {
    val op = newOperation(new WritePropertyOperation(base, prop, value, propsFor(prop)))
    properties += prop -> op
    value
  }

  def newFunction(func: Value, args: Seq[Value]): Value = {
    newOperation(new Call(func, args))
  }

  def report(report: Debug.Report): Unit = {
    global.reports += report -> newOperation(new Summary(Seq.empty, properties, global.localScopeObj))
  }

  def split(condition: Value): (GatherState, GatherState) = {
    //condition.isBlockCondition = true
    (
      new GatherState(global, Block(condition, targetTruthValue = true) :: blockStack, /*conditions + (condition -> true),*/ registers, properties),
      new GatherState(global, Block(condition, targetTruthValue = false) :: blockStack, /*conditions + (condition -> false),*/ registers, properties)
    )
  }

  private def newOperation[T <: Operation](op: T): T = {
    op.precondition = blockStack.headOption
    global.add(op)
    op
  }
}

case class Block(condition: Value, targetTruthValue: Boolean) {
  assert(condition.asPrecondition.forall(_ == condition))
  condition.asPrecondition = Some(condition)

  def concrete: Boolean = condition.evaluated.isConcrete
  def active: Boolean = condition.evaluated.truthValue.canBe(targetTruthValue)

  override def toString: String = (!targetTruthValue).pen("!") + condition
}


object GatherState {
  def empty = new GatherState(new Global, Nil, Map.empty, Map.empty)

  def withRand: GatherState = {
    val state = empty
    val rand = state.constant(DfConcreteInternalFunc("rand"))
    state.writeProp(state.global.localScope, "rand", rand)
    state
  }

  class Global {
    //var allVariables: List[Value] = List.empty
    val localScopeObj = new DfConcreteObjectRef("localScope")
    val localScope = new Constant(localScopeObj)
    val propertyInit = new PropertyInit
    val trueConst = new Constant(DfTrue)
    val reports = mutable.Map.empty[Debug.Report, Summary]
    val operations: mutable.Buffer[Operation] = mutable.Buffer.empty

    locally {
      add(propertyInit)
      add(localScope)
      add(trueConst)
    }

    def add(op: Operation): Unit = {
      op.index = operations.size
      operations += op
    }
  }


  def unify(states: Iterable[GatherState]): GatherState = {
    val global = states.head.global
    val minBlockStack = states.iterator.map(_.blockStack.length).min

    val newBlockStack = states.iterator.map(_.blockStack.takeRight(minBlockStack))
      .reduceLeft {
        (newBlockStack, nextBlockStack) =>
          newBlockStack.reverse.zip(nextBlockStack.reverse).takeWhile(==).map(_._1).reverse
      }

    val registers = states.iterator
      .map(_.registers)
      .reduce {
        (a, b) => a.mergeWith(b) {
          (r1, r2) => r1 | r2
        }
      }


//    val properties = states.iterator
//      .map(_.properties.view.mapValues(Set(_)).toMap)
//      .reduce {
//        (a, b) => a.mergeWith(b) {
//          (r1, r2) => r1 | r2
//        }
//      }.view
//      .mapValues(_.toSeq)
//      .map {
//        case (prop, Seq(source)) => prop -> source
//        case (prop, sources) =>
//
//          prop -> new PropertyPhiOperation(prop, ???)
//      }
//      .toMap

    val propsWithStates =
      for {
        state <- states.iterator
        prop <- state.properties
      } yield (prop._1, prop._2, state)

    val trueBlock = Block(global.trueConst, targetTruthValue = true)
    lazy val phiOp = {
      val op = new PropertyPhiOperation
      global.add(op)
      op
    }

    val properties = propsWithStates
      .toSeq
      .groupBy(_._1)
      .view
      .mapValues {
        case (_, source, state) +: rest if rest.forall(_._2 == source) => source
        case all =>
          phiOp.forProperty(
            all.head._1,
            all.map { case (_, source, state) => state.blockStack.headOption.getOrElse(trueBlock) -> source }
          )
      }
      .toMap

    val head = states.head
    new GatherState(head.global, newBlockStack, registers, properties)
  }
}