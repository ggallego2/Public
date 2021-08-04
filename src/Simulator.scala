import CustomerType._

import scala.math._
import scala.util.Random

case class Statistics(maxQueue: Int, avgQueue: Double, maxWait: Int, avgWait: Double)

case class BankQueue(queue: List[Int], busyTime: Int, queueTime: Int)

case class SimulatorCount(timeLeft: Int, interactions: Int) {
  def update: SimulatorCount = SimulatorCount(this.timeLeft - 1, this.interactions + 1)
}

case class QueueLengthCount(maxQueueLength: Int, queueLengthSum: Int)

case class ClientWaitCount(maxWaitTime: Int, waitTimeSum: Int, clientCount: Int)

case class Simulator(alpha: Double = 100, rho: Int = 200, customerType: CustomerType, maxTime: Int = 500) {
  val r: Random.type = scala.util.Random

  def simulate: Statistics = {

    epoch(BankQueue(List.empty, 0, 0), SimulatorCount(maxTime, 0), QueueLengthCount(0, 0), ClientWaitCount(0, 0, 0))
  }

  def epoch(bankQueue: BankQueue, simulatorCount: SimulatorCount, queueLengthCount: QueueLengthCount, clientWaitCount: ClientWaitCount): Statistics = {

    val uniformDist = r.nextFloat()

    val newMaxLength = max(bankQueue.queue.size, queueLengthCount.maxQueueLength)
    val newQueueLengthCount = QueueLengthCount(newMaxLength, queueLengthCount.queueLengthSum + bankQueue.queue.size)

    (simulatorCount.timeLeft, bankQueue.busyTime) match {

      //End of simulation
      case (0, _) =>

        Statistics(newMaxLength, queueLengthCount.queueLengthSum / simulatorCount.interactions.toDouble,
          clientWaitCount.maxWaitTime, clientWaitCount.waitTimeSum / clientWaitCount.clientCount.toDouble)

      //Next in queue gets attended
      case (_, 0) if bankQueue.queue.nonEmpty =>

        val attendedWaitTime = bankQueue.queue.head

        val notEmptyQueueBusyTime = if (bankQueue.queue.tail.nonEmpty) calculateBusyTime else 0

        val newWaitClientCount = ClientWaitCount(max(clientWaitCount.maxWaitTime, attendedWaitTime), clientWaitCount.waitTimeSum + attendedWaitTime, clientWaitCount.clientCount + 1)

        if (uniformDist < expectedQueueChance(bankQueue.queueTime))
          epoch(
            BankQueue(updateQueue(bankQueue.queue.tail) :+ 0, calculateBusyTime, 1),
            simulatorCount.update,
            newQueueLengthCount,
            newWaitClientCount)

        else
          epoch(
            BankQueue(updateQueue(bankQueue.queue.tail), notEmptyQueueBusyTime, bankQueue.queueTime + 1),
            simulatorCount.update,
            newQueueLengthCount,
            newWaitClientCount)

      //The customer is still being attended
      case (_, _) =>

        val emptyQueueBusyTime = if (bankQueue.queue.isEmpty) calculateBusyTime else max(bankQueue.busyTime - 1, 0)

        val newWaitClientCount = ClientWaitCount(clientWaitCount.maxWaitTime, clientWaitCount.waitTimeSum, clientWaitCount.clientCount)

        if (uniformDist < expectedQueueChance(bankQueue.queueTime))
          epoch(
            BankQueue(updateQueue(bankQueue.queue) :+ 0, emptyQueueBusyTime, 1),
            simulatorCount.update,
            newQueueLengthCount,
            newWaitClientCount)

        else
          epoch(
            BankQueue(updateQueue(bankQueue.queue), max(bankQueue.busyTime - 1, 0), bankQueue.queueTime + 1),
            simulatorCount.update,
            newQueueLengthCount,
            newWaitClientCount)
    }
  }

  def expectedQueueChance(t: Int) =
    1 - exp(-t / alpha)

  def calculateBusyTime = {
    val params = customerType match {
      case Yellow => (2, 5)
      case Blue => (5, 1)
      case Red => (2, 2)
    }

    val x = r.nextFloat()


    (rho * pow(x, params._1 - 1) * pow(1 - x, params._2 - 1)).round.toInt
  }

  def updateQueue(value: List[Int]): List[Int] =
    value.map(_ + 1)
}

object StatisticsOrdering extends Ordering[Statistics] {
  def compare(a: Statistics, b: Statistics) = a.maxWait - a.avgWait compare b.maxWait - b.avgWait
}

object StatisticsAbsOrdering extends Ordering[Statistics] {
  def compare(a: Statistics, b: Statistics) = (a.maxWait / a.avgWait) * 1000 compare (b.maxWait / b.avgWait) * 1000
}

//
//Simulation
//
for (a <- 1 to 20) {
  val maxTime = a * 100

  println(s"\n\n Simulate with max time of $maxTime:\n")
  val yellowCustomerSimulation = Simulator(customerType = Yellow)
  val redCustomerSimulation = Simulator(customerType = Red)
  val blueCustomerSimulation = Simulator(customerType = Blue)

  val yellowStatistics = yellowCustomerSimulation.simulate

  println("\nYellow Customer:")
  println(s"Simulation: $yellowCustomerSimulation")
  println(s"Average waiting time: ${yellowStatistics.avgWait}")
  println(s"Maximum waiting time: ${yellowStatistics.maxWait}")
  println(s"Average queue length: ${yellowStatistics.avgQueue}")
  println(s"Maximum queue length: ${yellowStatistics.maxQueue}")

  val redStatistics = redCustomerSimulation.simulate

  println("\nRed Customer:")
  println(s"Simulation: $redCustomerSimulation")
  println(s"Average waiting time: ${redStatistics.avgWait}")
  println(s"Maximum waiting time: ${redStatistics.maxWait}")
  println(s"Average queue length: ${redStatistics.avgQueue}")
  println(s"Maximum queue length: ${redStatistics.maxQueue}")

  val blueStatistics = blueCustomerSimulation.simulate;

  println("\nBlue Customer:")
  println(s"Simulation: $blueCustomerSimulation")
  println(s"Average waiting time: ${blueStatistics.avgWait}")
  println(s"Maximum waiting time: ${blueStatistics.maxWait}")
  println(s"Average queue length: ${blueStatistics.avgQueue}")
  println(s"Maximum queue length: ${blueStatistics.maxQueue}")

  println("\nResults:")
  println(s"Average yellow waiting time: ${yellowStatistics.avgWait}")
  println(s"Maximum yellow waiting time: ${yellowStatistics.maxWait}")
  println(s"Average red queue length: ${redStatistics.avgQueue}")
  println(s"Maximum red queue length: ${redStatistics.maxQueue}")

  val color = Map(yellowStatistics -> Yellow, redStatistics -> Red, blueStatistics -> Blue)
  val minDiff = color.keys.min(StatisticsOrdering)

  println(s"Closest value between the average and maximum customer waiting times colour: ${color(minDiff)}")

  val absColor = Map(yellowStatistics -> Yellow, redStatistics -> Red, blueStatistics -> Blue)
  val absMinDiff = absColor.keys.min(StatisticsAbsOrdering)

  println(s"Closest absolute value between the average and maximum customer waiting times colour: ${color(absMinDiff)}")

}

object CustomerType extends Enumeration {
  type CustomerType = Value
  val Yellow, Red, Blue = Value
}