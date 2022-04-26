package circuitbreaker

/**
 * Interface for a simple Circuit Breaker.
 *
 * The protect method takes a task and executes
 * it in a manner that may change the state of
 * the Circuit Breaker depending on the outcome
 * of the task.
 *
 * @author <a href="mailto:adam@evolvedbinary.com">Adam Retter</a>
 */
trait CircuitBreaker {

  /**
   * The name of the Circuit Breaker.
   */
  def name: String

  /**
   * Add a listener to the Circuit Breaker.
   *
   * @param listener the listener to add
   */
  def addListener(listener: Listener): Unit

  /**
   * Protect a Task with the Circuit Breaker.
   *
   * @tparam U The type of the result that the task produces.
   *
   * @param task the task to protect.
   *
   * @return the result of the task.
   *
   * @throws ExecutionRejectedException if the Circuit Breaker is in the Open state,
   *                                    or in the Half-Open state and this is not
   *                                    the first task after transistioning to the Half-Open state.
   */
  @throws[ExecutionRejectedException]
  def protect[U](task: () => U): ProtectResult[U]
}

/**
 * Result of calling [[CircuitBreaker.protect[U](() => U)]].
 */
sealed abstract class ProtectResult[T] {
  def isProtected: Boolean
  def isRejected: Boolean = !isProtected

  def fold[U](fe: String => U, ft: T => U): U = this match {
    case TaskWithFuse(t) => ft(t)
    case RejectedTask(message)  => fe(message)
  }
}
/**
 * Task has been accepted and is now protected with a "fuse".
 *
 * @param t the value
 */
case class TaskWithFuse[T](t: T) extends ProtectResult[T] {
  override val isProtected: Boolean = true
}
/**
 * Task has been rejected.
 *
 * @param message a description of why the task was rejected.
 */
case class RejectedTask[T](message: String) extends ProtectResult[T] {
  override val isProtected: Boolean = false
}

/**
 * The observable state of the Circuit Breaker.
 */
object State extends Enumeration {
  type State = Value
  val CLOSED, OPEN, HALF_OPEN = Value
}

/**
 * The internal state of the Circuit Breaker.
 *
 * Similar to {@link State} except internally
 * we have an additional RESET_TIMEOUT state
 * to help differentiate between entering
 * the HALF_OPEN state and being in the
 * HALF_OPEN state.
 */
private[circuitbreaker] object InternalState extends Enumeration {
  type InternalState = Value
  val CLOSED, OPEN, RESET_TIMEOUT, HALF_OPEN = Value

  /**
   * Maps from a State to an Internal State.
   *
   * @param state the state.
   *
   * @return the Internal State.
   */
  def from(state: State.State): InternalState = state match {
    case State.CLOSED => InternalState.CLOSED
    case State.OPEN => InternalState.OPEN
    case State.HALF_OPEN => InternalState.HALF_OPEN
  }
}

/**
 * Thrown when a Circuit Breaker rejects an incoming task.
 *
 * @param message the exception message.
 */
class ExecutionRejectedException(message: String) extends Throwable(message)

/**
 * Interface for a Listener which listens
 * to events produced by state changes in
 * a Circuit Breaker.
 */
trait Listener {
  def onClosed() : Unit
  def onHalfOpen() : Unit
  def onOpen() : Unit
}
