package ch10

import cats.effect.{ExitCode, IO, IOApp}
import io.github.timwspence.cats.stm.{STM, TVar}
import cats.syntax.flatMap._
import cats.syntax.either._
import ch10.tchan.TChan

object tchan {
  type TVarList[A] = TVar[TList[A]]

  sealed trait TList[+A] {
    def isEmpty: Boolean
  }
  case object TNil extends TList[Nothing] {
    def isEmpty: Boolean = true
  }
  case class TCons[A](a: A, as: TVarList[A]) extends TList[A] {
    def isEmpty: Boolean = false
  }
  object TList {
    def tNil[A]: TList[A] = TNil
    def tCons[A](a: A, as: TVarList[A]): TList[A] = TCons(a, as)
  }
  import TList._

  case class TChan[A](readVar: TVar[TVarList[A]], writeVar: TVar[TVarList[A]]) {
    def read: STM[A] = (for {
      listHead <- readVar.get
      head     <- listHead.get
    } yield head) flatMap {
      case TNil        => STM.retry
      case TCons(h, t) => readVar.set(t) >> STM.pure(h)
    }
    def write(a: A): STM[Unit] = for {
      newListEnd <- TVar.of(tNil[A])
      listEnd    <- writeVar.get
      _          <- writeVar.set(newListEnd)
      _          <- listEnd.set(tCons(a, newListEnd))
    } yield ()

    def duplicate: STM[TChan[A]] = for {
      hole       <- writeVar.get
      newReadVar <- TVar.of(hole)
    } yield TChan(newReadVar, writeVar)

    def unGet(a: A): STM[Unit] = for {
      listHead <- readVar.get
      newHead  <- TVar.of(tCons(a, listHead))
      _        <- readVar.set(newHead)
    } yield ()

    def isEmpty: STM[Boolean] = for {
      listHead <- readVar.get
      head     <- listHead.get
    } yield head.isEmpty
  }
  def newTChan[A]: STM[TChan[A]] = for {
    hole  <- TVar.of(tNil[A])
    read  <- TVar.of(hole)
    write <- TVar.of(hole)
  } yield TChan(read, write)

  def readEither[A, B](a: TChan[A], b: TChan[B]): STM[Either[A, B]] = {
    a.read.map(_.asLeft[B]) orElse b.read.map(_.asRight[A])
  }
}
object TChanMain extends IOApp {
  import STM._
  def print[A](a: A) = IO { println(s"$a") }

  def run(args: List[String]): IO[ExitCode] = for {
    c1 <- atomically[IO](tchan.newTChan[Char])
    _  <- atomically[IO](c1 write 'a')
    _  <- atomically[IO](c1.read)      >>= print
    _  <- atomically[IO](c1.isEmpty)   >>= print
    _  <- atomically[IO](c1 unGet 'a') >>= print
    _  <- atomically[IO](c1.isEmpty)   >>= print
    _  <- atomically[IO](c1.read)      >>= print
    c2 <- atomically[IO](c1.duplicate)
    _  <- atomically[IO](c1 write 'b')
    _  <- atomically[IO](c1.read)      >>= print
    _  <- atomically[IO](c2.read)      >>= print

  } yield ExitCode.Success
}