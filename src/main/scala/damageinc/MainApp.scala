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
      f <- countdown(10, 5, waiter).start
      _ <- IO.sleep(5.seconds)
      _ <- waiter.complete(())
      _ <- f.join
      _ <- IO.println("blast off!")
    } yield ()
}
