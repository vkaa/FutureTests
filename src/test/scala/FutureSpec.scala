import org.scalatest._

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

class FutureSpec extends FlatSpec with Matchers {

  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

  it should "print currentDirectory" in {
    val currentDirectory = new java.io.File(".").getCanonicalPath
    println(s"Scala version: ${currentDirectory}")
//    using(io.Source.fromFile(currentDirectory + "/build.sbt")) { source => {
//      for (line <- source.getLines) {
//        println(line)
//      }
//    }}
//    using(io.Source.fromFile(currentDirectory + "/build.sbt")) { source => {
//      for (line <- source.getLines.dropWhile(line => !line.startsWith("scalaVersion"))) {
//        println(line)
//      }
//    }}
//    using(io.Source.fromFile(currentDirectory + "/build.sbt")) { source => {
//      for (line <- source.getLines.find(_.startsWith("scalaVersion"))) {
//        println(line)
//      }
//    }}

    using(io.Source.fromFile(currentDirectory + "/build.sbt")) { source => {
      for ( version <- source.getLines.find(_.startsWith("scalaVersion"))) {
        println(version)
//        val versionPattern = ".*([0-9]+)".r
//        val versionPattern = "scalaVersion.*".r
        val versionPattern = "[0-9]+".r
        val versions = versionPattern.findAllIn(version)
//        versions.map(s => println(s))
        for ( v <- versions ) {
          println(v)
        }
      }
    }}
//    val versionPattern = """([0-9]+)\.([0-9]+)""".r
//    using(io.Source.fromFile(currentDirectory + "/build.sbt")) { source => {
//      for ( version <- source.getLines.find(_.startsWith("scalaVersion")).map(_ match {
//        case versionPattern(major, minor) => (major, minor)
//        case _ => ("x", "y")
//      })) {
//        println(version)
//      }
//    }}

  }

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
    println(s"fut type: ${fut.getClass}")
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
    println(s"fut type: ${fut.getClass}")
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

  sealed trait ListFp[+A]
  case object NilFp extends ListFp[Nothing]
  case class ConsFp[+A](head: A, tail: ListFp[A]) extends ListFp[A]

  object ListFp {
    def apply[A](as: A*): ListFp[A] =
      if (as.isEmpty) NilFp
      else ConsFp(as.head, apply(as.tail: _*))
  }

  def foldRightFp[A,B](as: ListFp[A], z: B)(f: (A, B) => B): B = as match {
    case NilFp => z
    case ConsFp(x, xs) => f(x, foldRightFp(xs, z)(f))
  }

  it should "use foldRight" in {
    val folded = foldRightFp(ListFp(1, 2, 3), NilFp: ListFp[Int])(ConsFp(_, _))
    folded match {
      case ConsFp(x, xs) => {
        x should be (1)
        xs match {
          case ConsFp(x, xs) => {
            x should be (2)
            xs match {
              case ConsFp(x, xs) => {
                x should be (3)
                xs should be (NilFp)
              }
              case _  => fail("Folded third element")
            }
          }
          case _  => fail("Folded second element")
        }
      }
      case _ => fail("Folded first element")
    }
  }
}
