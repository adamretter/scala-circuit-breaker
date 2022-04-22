package circuitbreaker

trait CircuitBreaker {

  def name: String

  protected object InternalState extends Enumeration {
    type InternalState = Value
    val CLOSED, OPEN, RESET_TIMEOUT, HALF_OPEN = Value

    def from(state: State.State): InternalState = state match {
      case State.CLOSED => InternalState.CLOSED
      case State.OPEN => InternalState.OPEN
      case State.HALF_OPEN => InternalState.HALF_OPEN
    }
  }

  def addListener(listener: Listener)
  def protect[U](task: () => U): U
}

object State extends Enumeration {
  type State = Value
  val CLOSED, OPEN, HALF_OPEN = Value
}

class ExecutionRejectedException(message: String) extends Throwable(message)

trait Listener {
  def onClosed() : Unit
  def onHalfOpen() : Unit
  def onOpen() : Unit
}
