package ch10

import cats.{Monad, Traverse}
import cats.effect.{ConcurrentEffect, Effect, ExitCode, Fiber, IO, IOApp}
import io.github.timwspence.cats.stm.{STM, TMVar}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.instances.list._
import cats.syntax.either._
import STM.atomically

case class AsyncT[F[_]: ConcurrentEffect, A](fiber: Fiber[F, Unit], tmvar: TMVar[Either[Throwable, A]]) {
  def waitF: F[A] = atomically(waitSTM)

  def waitCatchSTM: STM[Either[Throwable, A]] = tmvar.read

  def waitSTM: STM[A] = waitCatchSTM flatMap {
    case Left(t)  => STM.abort[A](t)
    case Right(a) => STM.pure(a)
  }

  def cancel: F[Unit] = fiber.cancel
}
object AsyncT {
  def waitEither[F[_]: Effect, A, B](a: AsyncT[F, A], b: AsyncT[F, B]): F[Either[A, B]] =
    atomically {
      STM.orElse(a.waitSTM.map(_.asLeft[B]), b.waitSTM.map(_.asRight[A]))
    }
  def waitAny[F[_]: Effect, A](asyncs: List[AsyncT[F, A]]): F[A] =
    atomically((asyncs map (_.waitSTM)).foldRight(STM.retry[A])(STM.orElse))


}
object GetUrlsMain extends IOApp {
  import AsyncT._

  def getURL[F[_]: Effect](s: String): F[String] = ???

  type ThreadId = Long
  val sites = List(
    "http://www.google.com",
    "http://www.bing.com",
    "http://www.yahoo.com",
    "http://www.wikipedia.com/wiki/Spade",
    "http://www.wikipedia.com/wiki/Shovel")

  def newEmptyTMVarF[F[_]: Effect, T]: F[TMVar[T]] = atomically(TMVar.empty)

  def forkFinally[F[_], A](
    action: F[A],
    andthen: Either[Throwable, A] => F[Unit]
  )(implicit F: ConcurrentEffect[F])
  : F[Fiber[F, Unit]] = F.start(F.attempt(action) >>= andthen) // TODO check mask

  def async[F[_], A](action: F[A])(implicit F: ConcurrentEffect[F]): F[AsyncT[F, A]] = for {
    tmVar <- newEmptyTMVarF[F, Either[Throwable, A]]
    fiber <- forkFinally[F, A](action, e => STM.atomically(tmVar.put(e)))
  } yield AsyncT(fiber, tmVar)

  def run(args: List[String]): IO[ExitCode] = {
    def download(url: String) = getURL[IO](url).map((url, _))
    for {
      as <- sites.traverse(a => async(download(a)))
      c  <- waitAny[IO, (String, String)](as)
      _  <- IO { println(f"${c._1} was first (${ c._2 } bytes})") }
      _  <- as.map(a => a.waitF).sequence
    } yield ExitCode.Success
  }
}
