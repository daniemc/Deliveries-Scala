package co.com.s4n.deliveries.infrastructure

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

object DeliveriesExecutor {
  def buildExecutor(threadNumber: Int): ExecutionContext = {
    implicit val exe = new ExecutionContext {
      val threadPool = Executors.newFixedThreadPool(threadNumber)
      def execute(runnable: Runnable) {
        threadPool.submit(runnable)
      }
      def reportFailure(t: Throwable) {}
    }
    exe
  }
}
