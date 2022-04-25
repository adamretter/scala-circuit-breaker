package circuitbreaker

trait TestableCircuitBreaker extends CircuitBreaker {
  /**
   * Package-private method for use in Tests only.
   *
   * Set the Internal State.
   *
   * @param internalState the Internal State to set
   */
  private[circuitbreaker] def setInternalState(internalState: InternalState.InternalState): Unit

  /**
   * Package-private method for use in Tests only.
   *
   * Get the Internal State.
   *
   * @return the Internal State
   */
  private[circuitbreaker] def getInternalState(): InternalState.InternalState

  /**
   * Package-private method for use in Tests only.
   *
   * Get the Max Failures.
   *
   * @return the max failures.
   */
  private[circuitbreaker] def getMaxFailures(): Int
}

trait TestableCircuitBreakerFactory {
  def apply(maxFailures: Int = 3, initialState: State.State) : TestableCircuitBreaker
}
