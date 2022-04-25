package circuitbreaker

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.UUID
import scala.concurrent.duration.DurationInt

class CircuitBreakerSpec extends AnyWordSpec with Matchers {

  "A CircuitBreaker" when {

    "in the CLOSED state" should {
      "return a result from a successful task" in {
        val successResult = s"OK-${uuid()}"
        val actualResult = TestCircuitBreaker().protect(() => successResult)
        actualResult mustEqual successResult
      }

      "return the exception from an unsuccessful task" in {
        val failureResult = OperationFailedException()
        an [OperationFailedException] mustBe thrownBy {
          TestCircuitBreaker().protect(() => throw failureResult)
        }
      }

      "switch to the OPEN state when maxFailures is exceeded" in {
        val breaker = TestCircuitBreaker()
        val countingListener = CountingListener()
        breaker.addListener(countingListener)

        // throw OperationFailedException upto maxFailures
        val failureResult = OperationFailedException()
        for (_ <- 0 to maxFailures) {
          an [OperationFailedException] mustBe thrownBy {
            breaker.protect(() => throw failureResult)
          }
        }

        // next OperationFailedException should exceed maxFailures, causing the transition to OPEN state, and therefore throwing ExecutionRejectedException
        val successResult = s"OK-${uuid()}"
        an [ExecutionRejectedException] mustBe thrownBy {
          breaker.protect(() => successResult)
        }

        countingListener.closed mustBe 0
        countingListener.halfOpened mustBe 0
        countingListener.opened mustBe 1
      }
    }

    "in the OPEN state" should {
      "reject all tasks" in {
        val successResult = s"OK-${uuid()}"
        an [ExecutionRejectedException] mustBe thrownBy {
          TestOpenCircuitBreaker().protect(() => successResult)
        }
      }
    }

    "in the RESET_TIMEOUT state" should {
      "accept the first task, and switch to HALF_OPEN and then CLOSED if task succeeds" in {
        val breaker = TestResetTimeoutCircuitBreaker()
        val countingListener = CountingListener()
        breaker.addListener(countingListener)

        val successResult = s"OK-${uuid()}"
        val actualResult = breaker.protect(() => successResult)
        actualResult mustEqual successResult
        breaker.getInternalState() mustEqual InternalState.CLOSED

        countingListener.halfOpened mustEqual 1
        countingListener.closed mustEqual 1
      }

      "accept the first task, and switch to HALF_OPEN and then OPEN if task fails" in {
        val breaker = TestResetTimeoutCircuitBreaker()
        val countingListener = CountingListener()
        breaker.addListener(countingListener)

        val failureResult = OperationFailedException()
        an [OperationFailedException] mustBe thrownBy {
          breaker.protect(() => throw failureResult)
        }
        breaker.getInternalState() mustEqual InternalState.OPEN

        countingListener.halfOpened mustEqual 1
        countingListener.opened mustEqual 1
      }
    }

    "in the HALF_OPEN state" should {
      "reject all tasks" in {
        val successResult = s"OK-${uuid()}"
        an [ExecutionRejectedException] mustBe thrownBy {
          TestHalfOpenCircuitBreaker().protect(() => successResult)
        }
      }
    }
  }

  val maxFailures = 3

  def TestCircuitBreaker() = ThreadSafeCircuitBreaker(maxFailures, resetTimeout = 10.second, exponentialBackoffFactor = 1, maxResetTimeout = 90.seconds)

  def TestOpenCircuitBreaker() = {
    ThreadSafeCircuitBreaker(maxFailures, resetTimeout = 10.second, exponentialBackoffFactor = 1, maxResetTimeout = 90.seconds, State.OPEN)
  }

  def TestResetTimeoutCircuitBreaker() = {
    val cb = ThreadSafeCircuitBreaker(maxFailures, resetTimeout = 10.second, exponentialBackoffFactor = 1, maxResetTimeout = 90.seconds)
    cb.setInternalState(InternalState.RESET_TIMEOUT)
    cb
  }

  def TestHalfOpenCircuitBreaker() = {
    val cb = ThreadSafeCircuitBreaker(maxFailures, resetTimeout = 10.second, exponentialBackoffFactor = 1, maxResetTimeout = 90.seconds)
    cb.setInternalState(InternalState.HALF_OPEN)
    cb
  }

  def uuid() = UUID.randomUUID().toString

  class OperationFailedException extends Throwable
  object OperationFailedException {
    def apply() = new OperationFailedException()
  }

  case class CountingListener(var closed: Int = 0, var halfOpened: Int = 0, var opened: Int = 0) extends Listener {
    override def onClosed(): Unit = closed += 1
    override def onHalfOpen(): Unit = halfOpened += 1
    override def onOpen(): Unit = opened += 1
  }
}
