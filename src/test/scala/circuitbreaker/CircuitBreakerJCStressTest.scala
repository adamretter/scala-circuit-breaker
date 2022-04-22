package circuitbreaker

import circuitbreaker.{ThreadSafeCircuitBreaker}
import org.openjdk.jcstress.annotations.{Actor, JCStressTest, Outcome, State}
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.{II_Result, LL_Result}

import scala.concurrent.duration.DurationInt

@JCStressTest
@Outcome(id = Array("IOException, IOException"), expect = ACCEPTABLE, desc = "Actor1 failed, and Actor2 failed.")
@State
class CircuitBreakerJCStressTest {

  val circuitBreaker = ThreadSafeCircuitBreaker(0, 1.milliseconds, 1, 0.seconds)

  @Actor
  def alwaysFails1(r: LL_Result): Unit = {
    try {
      val result = circuitBreaker.protect(() => throw new FailException)
      r.r1 = result
    } catch {
      case t: Throwable => r.r1 = t.getClass.getName
    }
  }

  @Actor
  def alwaysFails2(r: LL_Result): Unit = {
    try {
      val result = circuitBreaker.protect(() => throw new FailException)
      r.r2 = result
    } catch {
      case t: Throwable => r.r2 = t.getClass.getSimpleName
    }
  }

  class FailException extends Throwable
}
