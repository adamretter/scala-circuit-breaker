package circuitbreaker

import net.jcip.annotations.ThreadSafe

import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}
import java.util.{Timer, TimerTask}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

/**
 * A Circuit Breaker which is designed to be safely
 * accessed concurrently from multiple threads.
 *
 * @author <a href="mailto:adam@evolvedbinary.com">Adam Retter</a>
 */
@ThreadSafe
class ThreadSafeCircuitBreaker(val name: String, maxFailures: Int, resetTimeout: Duration, exponentialBackoffFactor: Int, maxResetTimeout: Duration, initialState: State.State) extends CircuitBreaker {

  import InternalState._

  private class ResetTimeoutTask extends TimerTask {
    // transition from OPEN -> RESET_TIMEOUT
    override def run(): Unit = state.compareAndSet(OPEN, RESET_TIMEOUT)
  }

  protected val state = new AtomicReference[InternalState](InternalState.from(initialState))
  private val failures = new AtomicInteger(0)
  private val resetTimer = new Timer(s"$name-ResetTimer")
  private val resetTimeoutTaskDelay = new AtomicLong(resetTimeout.toMillis)

  private val listeners = new CopyOnWriteArrayList[Listener]

  override def addListener(listener: Listener): Unit = {
    listeners.add(listener)
  }

  override def protect[U](task: () => U): U = {
    state.get() match {
      case CLOSED =>
        execClosedTask(task)

      case OPEN =>
        throw new ExecutionRejectedException("Circuit Breaker is Open")

      case RESET_TIMEOUT =>
        if (state.compareAndSet(RESET_TIMEOUT, HALF_OPEN)) {

          // notify listeners of state change
          listeners.forEach(_.onHalfOpen())

          // try and process a single item to see if we can reset the CircuitBreaker
          attemptReset(task)

        } else {
          // state has been changed by another thread that preempted this thread, so try-again...
          protect(task)
        }

      case HALF_OPEN =>
        throw new ExecutionRejectedException("Circuit Breaker is Half-Open and attempting to reset")
    }
  }

  private def execClosedTask[U](task: () => U): U = {
    Try(task()) match {
      case Success(value) =>
        // reset the failures count
        failures.set(0)

        // return the value of the task
        value

      case Failure(exception) =>
        // increment the failures counter
        val failedCount = failures.incrementAndGet()

        // if maxFailures is exceeded
        if (failedCount > maxFailures) {
          // transition from CLOSED -> OPEN
          if (state.compareAndSet(CLOSED, OPEN)) {

            // set the Reset Timer
            resetTimer.schedule(new ResetTimeoutTask(), resetTimeoutTaskDelay.get())

            // notify listeners of state change
            listeners.forEach(_.onOpen())
          }
        }

        // return the exception of the task
        throw exception
    }
  }

  private def attemptReset[U](task: () => U): U = {
    Try(task()) match {
      case Success(value) =>

        // reset the resetTimeoutTaskDelay to the initial value of resetTimeout
        resetTimeoutTaskDelay.set(resetTimeout.toMillis)

        // reset the failures count
        failures.set(0)

        // transition from HALF_OPEN -> CLOSED
        if (state.compareAndSet(HALF_OPEN, CLOSED)) {
          // notify listeners of state change
          listeners.forEach(_.onClosed())
        } else {
          throw new IllegalStateException("Attempted to transition state HALF_OPEN -> CLOSED, but current state was not HALF_CLOSED")
        }

        // return the value of the task
        value

      case Failure(exception) =>
        // multiply the resetTimeoutTaskDelay by the exponentialBackoffFactor, up to the configured maxResetTimeout
        resetTimeoutTaskDelay.updateAndGet(value => Math.min(maxResetTimeout.toMillis, value * exponentialBackoffFactor))

        // transition from HALF_OPEN -> OPEN
        if (state.compareAndSet(HALF_OPEN, OPEN)) {
          // set the Reset Timer
          resetTimer.schedule(new ResetTimeoutTask(), resetTimeoutTaskDelay.get())

          // notify listeners of state change
          listeners.forEach(_.onOpen())
        }

        // return the exception of the task
        throw exception
    }
  }
}

object ThreadSafeCircuitBreaker {

  private val nextSerialNumber = new AtomicInteger()

  def apply(maxFailures: Int, resetTimeout: Duration, exponentialBackoffFactor: Int, maxResetTimeout: Duration): ThreadSafeCircuitBreaker = {
    apply(maxFailures, resetTimeout, exponentialBackoffFactor, maxResetTimeout, State.CLOSED)
  }

  def apply(maxFailures: Int, resetTimeout: Duration, exponentialBackoffFactor: Int, maxResetTimeout: Duration, initialState: State.State): ThreadSafeCircuitBreaker = {
    val name = s"CircuitBreaker-${nextSerialNumber.getAndIncrement()}"
    apply(name, maxFailures, resetTimeout, exponentialBackoffFactor, maxResetTimeout, initialState)
  }

  def apply(name: String, maxFailures: Int, resetTimeout: Duration, exponentialBackoffFactor: Int, maxResetTimeout: Duration): ThreadSafeCircuitBreaker = {
    apply(name, maxFailures, resetTimeout, exponentialBackoffFactor, maxResetTimeout, State.CLOSED)
  }

  def apply(name: String, maxFailures: Int, resetTimeout: Duration, exponentialBackoffFactor: Int, maxResetTimeout: Duration, initialState: State.State): ThreadSafeCircuitBreaker = {
    new ThreadSafeCircuitBreaker(name, maxFailures, resetTimeout, exponentialBackoffFactor, maxResetTimeout, initialState)
  }
}