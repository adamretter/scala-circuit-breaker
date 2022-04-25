package circuitbreaker

import net.jcip.annotations.NotThreadSafe

import java.util.{Timer, TimerTask}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

/**
 * A Circuit Breaker which is designed only to be accessed
 * from a single thread.
 *
 * @author <a href="mailto:adam@evolvedbinary.com">Adam Retter</a>
 */
@NotThreadSafe
class StandardCircuitBreaker(val name: String, maxFailures: Int, resetTimeout: Duration, exponentialBackoffFactor: Int, maxResetTimeout: Duration, initialState: State.State) extends CircuitBreaker {

  import InternalState._

  private class ResetTimeoutTask extends TimerTask {
    // transition from OPEN -> RESET_TIMEOUT
    override def run(): Unit = state = RESET_TIMEOUT
  }

  protected var state = InternalState.from(initialState)
  private var failures: Int = 0
  private val resetTimer = new Timer(s"$name-ResetTimer")
  private var resetTimeoutTaskDelay : Long = resetTimeout.toMillis

  private var listeners = List[Listener]()

  override def addListener(listener: Listener): Unit = {
    listeners = listeners :+ listener
  }

  override def protect[U](task: () => U): U = {
    state match {
      case CLOSED =>
        execClosedTask(task)

      case OPEN =>
        throw new ExecutionRejectedException("Circuit Breaker is Open")

      case RESET_TIMEOUT =>
        state = HALF_OPEN

        // notify listeners of state change
        listeners.map(_.onHalfOpen())

        // try and process a single item to see if we can reset the CircuitBreaker
        attemptReset(task)

      case HALF_OPEN =>
        throw new ExecutionRejectedException("Circuit Breaker is Half-Open and attempting to reset")
    }
  }

  private def execClosedTask[U](task: () => U): U = {
    Try(task()) match {
      case Success(value) =>
        // reset the failures count
        failures = 0

        // return the value of the task
        value

      case Failure(exception) =>
        // increment the failures counter
        failures += 1

        // if maxFailures is exceeded
        if (failures > maxFailures) {
          // transition from CLOSED -> OPEN
          state = OPEN

          // set the Reset Timer
          resetTimer.schedule(new ResetTimeoutTask(), resetTimeoutTaskDelay)

          // notify listeners of state change
          listeners.map(_.onOpen())
        }

        // return the exception of the task
        throw exception
    }
  }

  private def attemptReset[U](task: () => U): U = {
    Try(task()) match {
      case Success(value) =>

        // reset the resetTimeoutTaskDelay to the initial value of resetTimeout
        resetTimeoutTaskDelay = resetTimeout.toMillis

        // reset the failures count
        failures = 0

        // transition from HALF_OPEN -> CLOSED
        state = CLOSED

        // notify listeners of state change
        listeners.map(_.onClosed())

        // return the value of the task
        value

      case Failure(exception) =>
        // multiply the resetTimeoutTaskDelay by the exponentialBackoffFactor, up to the configured maxResetTimeout
        resetTimeoutTaskDelay = Math.min(maxResetTimeout.toMillis, resetTimeoutTaskDelay * exponentialBackoffFactor)

        // transition from HALF_OPEN -> OPEN
        state = OPEN

        // set the Reset Timer
        resetTimer.schedule(new ResetTimeoutTask(), resetTimeoutTaskDelay)

        // notify listeners of state change
        listeners.map(_.onOpen())

        // return the exception of the task
        throw exception
    }
  }
}

object StandardCircuitBreaker {

  private var nextSerialNumber = 0

  def apply(maxFailures: Int, resetTimeout: Duration, exponentialBackoffFactor: Int, maxResetTimeout: Duration): StandardCircuitBreaker = {
    apply(maxFailures, resetTimeout, exponentialBackoffFactor, maxResetTimeout, State.CLOSED)
  }

  def apply(maxFailures: Int, resetTimeout: Duration, exponentialBackoffFactor: Int, maxResetTimeout: Duration, initialState: State.State): StandardCircuitBreaker = {
    val name = s"CircuitBreaker-${nextSerialNumber}"
    nextSerialNumber += 1
    apply(name, maxFailures, resetTimeout, exponentialBackoffFactor, maxResetTimeout, initialState)
  }

  def apply(name: String, maxFailures: Int, resetTimeout: Duration, exponentialBackoffFactor: Int, maxResetTimeout: Duration): StandardCircuitBreaker = {
    apply(name, maxFailures, resetTimeout, exponentialBackoffFactor, maxResetTimeout, State.CLOSED)
  }

  def apply(name: String, maxFailures: Int, resetTimeout: Duration, exponentialBackoffFactor: Int, maxResetTimeout: Duration, initialState: State.State): StandardCircuitBreaker = {
    new StandardCircuitBreaker(name, maxFailures, resetTimeout, exponentialBackoffFactor, maxResetTimeout, initialState)
  }
}

