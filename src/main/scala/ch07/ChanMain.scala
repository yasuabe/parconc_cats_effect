package ch07

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.effect.concurrent.MVar
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}

trait UsesChan[F[_]] {
  implicit val F: Concurrent[F]
  implicit val C: Clock[F]

  type Stream[A] = MVar[F, Item[A]]
  case class Item[A](head: A, tail: Stream[A])

  case class Chan[A](reader: MVar[F, Stream[A]], writer: MVar[F, Stream[A]]) {
    def read = for {
      stream <- reader.take
      item   <- stream.read
      _      <- reader.put(item.tail)
    } yield item.head

    def write(a: A): F[Unit] = for {
      newHole <- MVar.empty[F, Item[A]]
      oldHole <- writer.take
      _       <- oldHole put Item(a, newHole)
      _       <- writer put newHole
    } yield ()

    def dup: F[Chan[A]] = for {
      hole      <- writer.read
      newReader <- MVar.of(hole)
    } yield copy(reader = newReader)
  }
  object Chan {
    def apply[A]: F[Chan[A]] = for {
      hole     <- MVar.empty[F, Item[A]]
      readVar  <- MVar.of(hole)
      writeVar <- MVar.of(hole)
    } yield Chan(readVar, writeVar)
  }
}
import parconc_util.InfoLogging._
import scala.concurrent.duration._

trait Program[F[_]] extends UsesChan[F] {
  implicit val T: Timer[F]

  def wait(d: Double): F[Unit] = T.sleep(d.second)
  def program: F[Unit] = for {
    w1 <- Chan[String]
    r2 <- w1.dup
    r3 <- w1.dup
    r1 =  (r2.read >>= ((s: String) => info[F](s"r1: $s"))) >> wait(1.0)
    _  <- F.start { wait(0.5) >> r1 >> r1 >> r1 }
    r2 =  (r3.read >>= ((s: String) => info[F](s"r2: $s"))) >> wait(1.5)
    _  <- F.start { r2 >> r2 >> r2 }
    w  =  (s: String) => w1.write(s) >> info[F](s"w: $s") >> wait(1.0)
    _  <- w("a") >> w("b") >> w("c")
  } yield ()
  //  sec
  //  0.0: w : a
  //  0.0: r2: a
  //  0.5: r1: a
  //  1.0: w : b
  //  1.5: r1: b
  //  1.5: r2: b
  //  2.0: w : c
  //  2.5: r1: c
  //  3.0: r2: c
}
object ChanMain extends IOApp with Program[IO] {
  val F: Concurrent[IO] = Concurrent[IO]
  val C: Clock[IO] = Clock[IO]
  val T: Timer[IO] = Timer[IO]
  override def run(args: List[String]): IO[ExitCode] = program as ExitCode.Success
}
