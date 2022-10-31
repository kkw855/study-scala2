package book.essential_effects

import cats.effect._
import cats._
import cats.implicits._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object IOComposition extends App {
  import cats.effect.unsafe.implicits.global

  val hello = IO(println(s"[${Thread.currentThread.getName}] Hello"))
  val world = IO(println(s"[${Thread.currentThread.getName}] World"))
  val hw1: IO[Unit] =
    for {
      _ <- hello
      _ <- world
    } yield ()

  val hw2: IO[Unit] =
    (hello, world).mapN((_, _) => ())

  hw1.unsafeRunSync()
  hw2.unsafeRunSync()
}

object ApplicativeFutures extends App {
  import scala.concurrent.ExecutionContext.Implicits.global

//  def fetchFirstName: Future[String] = {
//    println("fetching first name"); Future.successful("John")
//  }

  def fetchFirstName: Future[String] = Future.failed(new Exception("BOOM!"))

  def fetchLastName: Future[String] = {
    println("fetching last name"); Future.successful("Doe")
  }

  val eventualFullName: Future[String] =
    Applicative[Future].map2(fetchFirstName, fetchLastName)(_ + " " + _)
  val fullName: String = Await.result(eventualFullName, 1.second)
  println(fullName)
}

object ParallelExample1 extends App {}

object ParIO extends App {
  val ia: IO[Int] = IO(1)
  val ib: IO[String] = IO("world")

  def f(a: Int, b: String): String = s"$a $b"

  val ipa: IO.Par[Int] = Parallel[IO].parallel(ia)
}
