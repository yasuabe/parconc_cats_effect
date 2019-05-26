package ch07

import cats.effect._
import cats.effect.concurrent.MVar
import cats.syntax.flatMap._
import cats.syntax.functor._
import parconc_util.InfoLogging._

trait UsesLogger[F[_]] {
  implicit val F: Concurrent[F]
  implicit val C: Clock[F]

  sealed trait LogCommand
  case class Message(value: String) extends LogCommand
  case class Stop(value: MVar[F, Unit]) extends LogCommand

  case class Logger(m: MVar[F, LogCommand]) {
    def message(s: String): F[Unit] = m.put(Message(s))
    def start: F[Unit] = {
      def loop: F[Unit] = m.take flatMap {
        case Message(msg) => info(msg) >> loop
        case Stop(s)      => info("logger: stop") >> s.put(())
      }
      loop
    }
    def stop: F[Unit] = for {
      s <- MVar.empty[F, Unit]
      _ <- m.put(Stop(s))
      _ <- s.take
    } yield ()
  }

  def initLogger: F[Logger] = for {
    m <- MVar.empty[F, LogCommand]
    l =  Logger(m)
    _ <- F.start(l.start)
  } yield l
}

object LoggerMain extends IOApp with UsesLogger[IO] {
  val F: Concurrent[IO] = Concurrent[IO]
  val C: Clock[IO]      = Clock[IO]

  def run(args: List[String]): IO[ExitCode] = for {
    l <- initLogger
    _ <- l.message("hello")
    _ <- l.message("bye")
    _ <- l.stop
  } yield ExitCode.Success
}

