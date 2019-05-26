package parconc_util

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

import cats.Show
import cats.effect.{Clock, Sync}
import cats.implicits._

import scala.concurrent.duration.MILLISECONDS

object InfoLogging {
  implicit val instanceShow: Show[Instant] =
    i => i.atZone(ZoneId.systemDefault()).format(DateTimeFormatter.ofPattern("HH:mm:ss.SSS"))

  def info[F[_]](s: Any)(implicit F: Sync[F], C: Clock[F]): F[Unit] = for {
    tm <- C.realTime(MILLISECONDS).map(t => Instant.ofEpochMilli(t).show)
    th <- F.delay(Thread.currentThread().getId)
    _  <- F.delay(println(s"$tm (thread $th): $s"))
  } yield ()
}
