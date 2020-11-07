package damageinc

import java.util.concurrent.TimeoutException

import cats.effect.{IO, IOApp}
import cats.effect.kernel.Deferred
import cats.syntax.all._

import scala.concurrent.duration._

object ExampleFive extends IOApp.Simple {

  def countdown(n: Int, pause: Int, waiter: Deferred[IO, Unit]): IO[Unit] =
    IO.println(n) *> IO.defer {
      if (n == 0) IO.unit
      else if (n == pause)
        IO.println("paused...") *> waiter.get *> countdown(n - 1, pause, waiter)
      else countdown(n - 1, pause, waiter)
    }

  override def run: IO[Unit] =
    for {
      waiter <- IO.deferred[Unit]
      f      <- countdown(10, 5, waiter).start
      _      <- IO.sleep(5.seconds)
      _      <- waiter.complete(())
      _      <- f.join
      _      <- IO.println("blast off!")
    } yield ()
}

object TimeoutApp extends IOApp.Simple {

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val timeBomb = IO.sleep(duration) *> IO.raiseError(new TimeoutException())
    for {
      /*
      waiter <- IO.deferred[Unit]
      timer         = timeBomb.start.flatMap(fiber => waiter.get *> fiber.cancel *> fiber.joinAndEmbed(IO.unit))
      valueProducer = io.flatMap(a => waiter.complete(()).as(a))
      (a, _) <- IO.both(valueProducer, timer)
       */
      ret <- IO.race(io, timeBomb)
    } yield ret.fold(identity, identity)
  }

  override def run: IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- timeout(
        IO.println("STOP THE COUNT") *> IO.sleep(800.millis) *> IO.println(
          "Donnie, pack yer stuff"
        ),
        500.millis
      )
    } yield ()
}

object ParTraverseApp extends IOApp.Simple {

  def parTraverse[A, B](as: List[A])(f: A => IO[B]): IO[List[B]] = as match {
    case Nil => IO(List.empty[B])
    case x :: xs =>
      IO.both(f(x), parTraverse(xs)(f)).map { case (b, bs) =>
        b :: bs
      }
  }

  override def run: IO[Unit] = {
    def f(x: Int) = IO.sleep(1.second) *> IO.println("ze " + x) *> IO.pure(x)
    for {
      lst <- parTraverse((1 until 10).toList)(f)
      _ <- IO.println(lst)
    } yield ()
  }
}

trait Semaphore {
  def acquire: IO[Unit]
  def release: IO[Unit]
}

object Semaphore {
  def apply(permits: Int): IO[Semaphore] = ???
}



object SemaphoreApp extends IOApp.Simple {
  override def run: IO[Unit] = ???
}
