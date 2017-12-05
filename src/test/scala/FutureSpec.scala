import org.scalatest._

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

class FutureSpec extends FlatSpec with Matchers {

  it should "throw Exception for Future.failed" in {
    val futFailed = Future.failed(new Exception("Exception in failed future"))
    an [Exception] should be thrownBy {
      Await.result(futFailed, 1.seconds)
    }
  }

  it should "transform Future.failed" in {
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

  it should "transform Future.failed without exception" in {
    val futFailed = Future.failed(new Exception("Exception in failed future"))
    val transformed = futFailed.transform {
      case Success(r) => Try(identity())
      case Failure(e) => Try(e.getMessage)
    }
    val ret = Await.result(transformed, 1.seconds)
    ret should be ("Exception in failed future")
  }

  it should "return result for Future.successful" in {
    val fut = Future.successful(123)
//    println(s"fut: ${fut}")
    val toTry = fut.transform {
      case Success(r) => Try(r)
      case Failure(e) => Try(e.getMessage)
    }
//    println(s"toTry type: ${toTry.getClass}")
    val ret = Await.result(toTry, 1.seconds)
//    println(s"toTry result: ${ret}")
//    println(s"toTry result: ${ret.getClass}")
    ret should be (123)
  }

  it should "return Right for Future.successful" in {
    type IntStatus = Either[
      String,
      BigDecimal
      ]
    val fut = Future.successful(123)
    val toTry: Future[IntStatus] = fut.transform {
      case Success(r) => Try(Right(r))
      case Failure(e) => Try(Left(e.getMessage))
    }
    val ret = Await.result(toTry, 1.seconds)
    ret.isRight should be (true)
    ret match {
      case Left(_) =>  fail("Future.successfull should not be transformed to Left")
      case Right(v) => v should be (123)
    }
  }

  it should "return Left for Future.failed" in {
    type IntStatus = Either[
      String,
      BigDecimal
      ]
    val fut = Future.failed(new Exception("Exception in failed future"))
    val toTry: Future[IntStatus] = fut.transform {
      case Success(r) => Try(Right(r))
      case Failure(e) => Try(Left(e.getMessage))
    }
    val ret = Await.result(toTry, 1.seconds)
    ret.isLeft should be (true)
    ret match {
      case Left(v) =>  v should be ("Exception in failed future")
      case Right(_) => fail("Future.failed should not be transformed to Right")
    }
  }
}
