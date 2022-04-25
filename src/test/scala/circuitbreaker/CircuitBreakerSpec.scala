package circuitbreaker

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * Specs for a Circuit Breaker.
 *
 * @author <a href="mailto:adam@evolvedbinary.com">Adam Retter</a>
 */
class CircuitBreakerSpec extends AnyWordSpec with Matchers with CircuitBreakerBehaviours {

  private val maxFailures = 3

  // for each type of Circuit Breaker
  for (testableCircuitBreakerFactory <- Seq(TestableStandardCircuitBreaker, TestableThreadSafeCircuitBreaker)) {
    s"A ${testableCircuitBreakerFactory.getClass.getSimpleName}" when {

      inClosedState(TestableClosedCircuitBreaker(testableCircuitBreakerFactory))

      inOpenState(TestableOpenCircuitBreaker(testableCircuitBreakerFactory))

      inResetTimeoutState(TestableResetTimeoutCircuitBreaker(testableCircuitBreakerFactory))

      inHalfOpenState(TestableHalfOpenCircuitBreaker(testableCircuitBreakerFactory))
    }
  }

  private def TestableClosedCircuitBreaker(testableCircuitBreakerFactory: TestableCircuitBreakerFactory): TestableCircuitBreaker = testableCircuitBreakerFactory(maxFailures, State.CLOSED)

  private def TestableOpenCircuitBreaker(testableCircuitBreakerFactory: TestableCircuitBreakerFactory): TestableCircuitBreaker = testableCircuitBreakerFactory(maxFailures, State.OPEN)

  private def TestableResetTimeoutCircuitBreaker(testableCircuitBreakerFactory: TestableCircuitBreakerFactory): TestableCircuitBreaker = {
    val cb = testableCircuitBreakerFactory(maxFailures, State.CLOSED)
    cb.setInternalState(InternalState.RESET_TIMEOUT)
    cb
  }

  private def TestableHalfOpenCircuitBreaker(testableCircuitBreakerFactory: TestableCircuitBreakerFactory): TestableCircuitBreaker = {
    val cb = testableCircuitBreakerFactory(maxFailures, State.CLOSED)
    cb.setInternalState(InternalState.HALF_OPEN)
    cb
  }
}
