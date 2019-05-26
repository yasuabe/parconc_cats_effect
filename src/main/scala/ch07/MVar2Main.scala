package ch07

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

import cats.Show
import cats.effect.concurrent.MVar
import cats.effect.{Concurrent, ExitCode, IO, IOApp, Sync, Timer, Clock }
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.show._

import scala.concurrent.duration._

object MVar2Main extends IOApp {
  implicit val instanceShow: Show[Instant] =
    i => i.atZone(ZoneId.systemDefault()).format(DateTimeFormatter.ofPattern("HH:mm:ss.SSS"))

  def info[F[_]](s: Any)(implicit S: Sync[F], C: Clock[F]): F[Unit] = for {
    tm <- C.realTime(MILLISECONDS).map(t => Instant.ofEpochMilli(t).show)
    th <- S.delay(Thread.currentThread().getId)
    _ <- S.delay(println(s"$tm (thread $th): $s"))
  } yield ()

  def program[F[_]](implicit C: Concurrent[F], T: Timer[F]): F[Unit] = for {
    m <- MVar.empty[F, Char]
    _ <- info("start")
    _ <- C.start { m.put('x') >> info("put x") >> m.put('y') >> info("put y") }
    _ <- T.sleep(1.second) >> (m.take >>= info[F]) >> (m.take >>= info[F])
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    program[IO] as ExitCode.Success
}
