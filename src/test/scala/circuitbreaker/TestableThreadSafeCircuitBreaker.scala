package circuitbreaker

import scala.concurrent.duration.DurationInt

private[circuitbreaker] class TestableThreadSafeCircuitBreaker(maxFailures: Int = 3, initialState: State.State = State.CLOSED) extends ThreadSafeCircuitBreaker(
  name ="TestableStandardCircuitBreaker",
  maxFailures = maxFailures,
  resetTimeout = 10.seconds,
  exponentialBackoffFactor = 1,
  maxResetTimeout = 90.seconds,
  initialState) with TestableCircuitBreaker {

  private[circuitbreaker] override def setInternalState(internalState: InternalState.InternalState): Unit = {
    state.set(internalState)
  }

  private[circuitbreaker] override def getInternalState(): InternalState.InternalState = {
    state.get
  }
}

private[circuitbreaker] object TestableThreadSafeCircuitBreaker {
  def apply(maxFailures: Int = 3, initialState: State.State = State.CLOSED) = new TestableThreadSafeCircuitBreaker(maxFailures, initialState)
}
