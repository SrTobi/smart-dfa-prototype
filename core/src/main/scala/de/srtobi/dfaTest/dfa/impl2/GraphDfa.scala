package de.srtobi.dfaTest
package dfa
package impl2

import de.srtobi.dfaTest.cfg.{ControlFlowGraph, Debug}
import de.srtobi.dfaTest.dfa.impl2.GraphDfa.Task

import scala.collection.mutable

class GraphDfa(val reports: Map[Debug.Report, Summary],
               val operations: Array[Operation],
               val localScopeObj: DfConcreteObjectRef,
               val makeAssumptions: Boolean = true) {

  def gatherReports(reportF: (Debug.Report, Summary) => Unit): Unit = {
    evaluateAll(
      reports
        .iterator
        .map {
          case (report, summary) =>
            new Task {
              override def target: Operation = summary
              override def done(): Unit = reportF(report, summary)
            }
        }.toSeq
        .sortBy(_.target.index)
    )
  }

  def evaluateAll(tasks: Seq[Task]): Unit = {
    //var nextGuess = Option.empty[Value]
    //val enqueued = mutable.Buffer.empty[Value]
    val operationToTask = tasks.groupBy(_.target)
    var finishedSummaries = -1
    val workList = mutable.PriorityQueue.empty[Operation](Ordering.by(-_.index))
    val itemsInWorkList = mutable.Set.empty[Operation]

    def addToWorkList(operation: Operation): Unit = {
      if (!itemsInWorkList.contains(operation)) {
        itemsInWorkList += operation
        workList += operation
      }
    }

    operations.foreach(addToWorkList)


    def pushAssumption(cond: Value, targetTruthValue: Boolean): Unit = {
      operations.foreach(_.pushState())
      cond.setPropagateAssumptions(targetTruthValue)
      cond.setFilter(DfConcreteBoolean(targetTruthValue))
      operations.foreach(addToWorkList)
    }

    def popAssumption(): Unit =
      operations.foreach(_.popState())

    def processWithAssumption(cond: Value, targetTruthValue: Boolean): Unit = {
      val save = finishedSummaries
      pushAssumption(cond, targetTruthValue)
      processWorkList()
      popAssumption()
      finishedSummaries = save
    }

    def processWorkList(): Unit =
      while (workList.nonEmpty) {
        val cur = workList.dequeue()
        itemsInWorkList.remove(cur)

        if (cur.active) {
          cur.process()

          cur.asPrecondition match {
            case Some(pre) if makeAssumptions && !pre.isPropagatingAssumptions =>
              val evaluated = pre.evaluated
              if (!evaluated.isConcrete) {
                processWithAssumption(pre, targetTruthValue = true)
                processWithAssumption(pre, targetTruthValue = false)
              }
            case _ =>
          }

          if (cur.isInstanceOf[Summary] && cur.index > finishedSummaries) {
            finishedSummaries = cur.index
            for (tasks <- operationToTask.get(cur); task <- tasks) {
              task.done()
            }
          }
        }
      }

    processWorkList()

    /*val activator = new Activator {
      override def enqueue(value: Value): Unit = enqueued += value
      override def needsGuess(value: Value): Unit = {
        //assert(value.isBlockCondition)
        nextGuess = nextGuess match {
          case Some(prevNextGuess) =>
            val Seq(nextGuess, toEnqueue) = Seq(prevNextGuess, value).sortBy(_.index)
            enqueued += toEnqueue
            Some(nextGuess)
          case None =>
            Some(value)
        }
      }
      override def done(value: Value): Unit =
        for (tasks <- operationToTask.get(value); task <- tasks)
          task.done()
    }*/

    //nextGuess match {
    //  case Some(guess) => ???
    //  case None =>
    //}
  }

  def print(): Unit = {
    operations.map(_.toText).foreach(println)
  }
}


object GraphDfa {
  trait Task {
    def target: Operation
    def done(): Unit
  }

  def apply(cfg: ControlFlowGraph): GraphDfa = {
    val resultState = {
      val gatherDfa = new ConstraintAnalysis()
      val runner = new DfaRunner(gatherDfa)(cfg)
      runner.run()
      gatherDfa.result.get
    }
    val global = resultState.global
    new GraphDfa(global.reports.toMap, global.operations.toArray, global.localScopeObj)
  }

  def main(args: Array[String]): Unit = {
    val code =
      """
        |x = rand()
        |a = 1
        |if (x == 2) {
        |  a = 2
        |}
        |""".stripMargin
    val cfg = CfgTransformer.transformScriptWithReports(LangParser.parse(code), code)
    println(cfg.asmText())
    println("---------------------------")
    val dfa = GraphDfa(cfg)
    dfa.print()
    println("---------------------------")
    dfa.gatherReports {
      (report, summary) =>
        println(s"<<<< ${report.before.fold("before", "after")} line ${report.line} >>>>")
        printObjs(summary.summarize(), dfa.localScopeObj)
    }
  }

  def printObjs(objs: Map[DfConcreteObjectRef, Map[String, DfAbstractAny]], localScope: DfConcreteObjectRef): Unit = {
    explore(localScope) {
      (visit, obj) =>
        val (indent, end) =
          if (obj != localScope) {
            println(obj.toString + " = {")
            "  " -> "}\n"
          } else "" -> ""

        for ((prop, value) <- objs(obj)) {
          println(indent + prop + ": " + value)
          value.concreteObjectRefs.foreach(visit)
        }
        print(end)
    }
  }
}