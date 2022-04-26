package circuitbreaker

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.UUID

/**
 * Spec behaviours for a Circuit Breaker.
 *
 * @author <a href="mailto:adam@evolvedbinary.com">Adam Retter</a>
 */
trait CircuitBreakerBehaviours extends AnyWordSpec with Matchers {

  def inClosedState(newCircuitBreaker: => TestableCircuitBreaker): Unit = {
    "in the CLOSED state" should {
      "return a result from a successful task" in {
        val successResult = s"OK-${uuid()}"
        val actualResult = newCircuitBreaker.protect(() => successResult)
        actualResult mustEqual TaskWithFuse(successResult)
      }

      "return the exception from an unsuccessful task" in {
        val failureResult = OperationFailedException()
        an [OperationFailedException] mustBe thrownBy {
          newCircuitBreaker.protect(() => throw failureResult)
        }
      }

      "switch to the OPEN state when maxFailures is exceeded" in {
        val breaker = newCircuitBreaker
        val countingListener = CountingListener()
        breaker.addListener(countingListener)

        // throw OperationFailedException upto maxFailures
        val failureResult = OperationFailedException()
        for (_ <- 0 to breaker.getMaxFailures()) {
          an [OperationFailedException] mustBe thrownBy {
            breaker.protect(() => throw failureResult)
          }
        }

        // next OperationFailedException should exceed maxFailures, causing the transition to OPEN state, and therefore throwing ExecutionRejectedException
        val successResult = s"OK-${uuid()}"
        val actualResult = breaker.protect(() => successResult)
        actualResult mustBe a [RejectedTask[_]]

        countingListener.closed mustBe 0
        countingListener.halfOpened mustBe 0
        countingListener.opened mustBe 1
      }
    }
  }

  def inOpenState(newCircuitBreaker: => CircuitBreaker): Unit = {
    "in the OPEN state" should {
      "reject all tasks" in {
        val successResult = s"OK-${uuid()}"
        val actualResult = newCircuitBreaker.protect(() => successResult)
        actualResult mustBe a [RejectedTask[_]]
      }
    }
  }

  def inResetTimeoutState(newCircuitBreaker: => TestableCircuitBreaker): Unit = {
    "in the RESET_TIMEOUT state" should {
      "accept the first task, and switch to HALF_OPEN and then CLOSED if task succeeds" in {
        val breaker = newCircuitBreaker
        val countingListener = CountingListener()
        breaker.addListener(countingListener)

        val successResult = s"OK-${uuid()}"
        val actualResult = breaker.protect(() => successResult)
        actualResult mustEqual TaskWithFuse(successResult)
        breaker.getInternalState() mustEqual InternalState.CLOSED

        countingListener.halfOpened mustEqual 1
        countingListener.closed mustEqual 1
      }

      "accept the first task, and switch to HALF_OPEN and then OPEN if task fails" in {
        val breaker = newCircuitBreaker
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
  }

  def inHalfOpenState(newCircuitBreaker: => TestableCircuitBreaker): Unit = {
    "in the HALF_OPEN state" should {
      "reject all tasks" in {
        val successResult = s"OK-${uuid()}"
        val actualResult = newCircuitBreaker.protect(() => successResult)
        actualResult mustBe a [RejectedTask[_]]
      }
    }
  }

  private def uuid() = UUID.randomUUID().toString

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
