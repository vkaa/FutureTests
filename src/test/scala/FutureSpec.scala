import org.scalatest._

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

class FutureSpec extends FlatSpec with Matchers {

    "Future" should "transform" in {
      val futFailed = Future.failed(new Exception("Exception in failed future"))
      val transformed = futFailed.transform {
        case Success(r) => Try(identity())
        case Failure(e) => Try(e.getMessage)
      }
      println(s"transformed type: ${transformed.getClass}")
      val ret = Await.result(transformed, 1.seconds)
      println(s"transformed result: ${ret}")
      println(s"transformed result: ${ret.getClass}")
    }
}
