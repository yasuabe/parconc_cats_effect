package ch07

import cats.effect._
import cats.effect.concurrent.MVar
import cats.syntax.flatMap._
import cats.syntax.functor._
import parconc_util.InfoLogging._

import scala.concurrent.duration._

object MVar1Main extends IOApp {
  def program[F[_]](implicit C: Concurrent[F], T: Clock[F]): F[Unit] = for {
    m <- MVar.empty[F, Char]
    _ <- C.start { m.put('x') }
    r <- m.take
    _ <- info(r)
  } yield ()

  def program2[F[_]](implicit C: Concurrent[F], T: Timer[F]): F[Unit] = for {
    m <- MVar.empty[F, Char]
    _ <- C.start { T.sleep(3.second) >> m.put('x') }
    r <- info("y") >> m.take
    _ <- info(r)
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    program[IO] as ExitCode.Success
}
