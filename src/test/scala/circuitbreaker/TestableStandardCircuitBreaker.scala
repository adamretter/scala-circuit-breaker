package circuitbreaker

import scala.concurrent.duration.DurationInt

private[circuitbreaker] class TestableStandardCircuitBreaker(maxFailures: Int = 3, initialState: State.State = State.CLOSED) extends StandardCircuitBreaker(
  name ="TestableStandardCircuitBreaker",
  maxFailures = maxFailures,
  resetTimeout = 10.seconds,
  exponentialBackoffFactor = 1,
  maxResetTimeout = 90.seconds,
  initialState) with TestableCircuitBreaker {

  private[circuitbreaker] override def setInternalState(internalState: InternalState.InternalState): Unit = {
    state = internalState
  }

  private[circuitbreaker] override def getInternalState(): InternalState.InternalState = {
    state
  }

  private[circuitbreaker] override def getMaxFailures(): Int = maxFailures
}

private[circuitbreaker] object TestableStandardCircuitBreaker extends TestableCircuitBreakerFactory {
  override def apply(maxFailures: Int = 3, initialState: State.State = State.CLOSED) = new TestableStandardCircuitBreaker(maxFailures, initialState)
}
