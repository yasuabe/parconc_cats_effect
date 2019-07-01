package ch10

import cats.effect.Effect
import io.github.timwspence.cats.stm._
import cats.syntax.flatMap._
import cats.syntax.functor._

object WindowMain {
  import STM.atomically
  case class Desktop()
  case class Window()

  type UserFocus = TVar [Desktop]

  def getSTM[K, V](m: Map[K, V])(k: K): STM[V] =
    m.get(k).fold(STM.abort[V](new NoSuchElementException()))(STM.pure)

  case class Display(assoc: Map[Desktop, TVar[Set[Window]]]) {
    def windowsOn: Desktop => STM[TVar[Set[Window]]] = getSTM(assoc)
    def getWindows(f: UserFocus): STM[Set[Window]] = f.get >>= windowsOn >>= (_.get)
    def moveWindowSTM(win: Window, a: Desktop, b: Desktop): STM[Unit] = {
      for {
        ma <- windowsOn(a)
        mb <- windowsOn(b)
        wa <- ma.get
        wb <- mb.get
        _  <- ma.set(wa - win)
        _  <- mb.set(wb + win)
      } yield ()
    }
  }

  def swapWindows[F[_]: Effect](
    d: Display, w: Window, a: Desktop, v: Window, b: Desktop
  ): F[Unit] = atomically[F] {
    d.moveWindowSTM(w, a, b) >> d.moveWindowSTM(v, b, a)
  }
  def render[F[_]: Effect](wins: Set[Window]): F[Unit] = ???
  def rednderThread[F[_]: Effect](disp: Display, focus: UserFocus): F[Unit] = {
    def x(wins: Set[Window]): F[Unit] = loop(wins)
    def loop(wins: Set[Window]): F[Unit] = for {
      _ <- render(wins)
      next <- atomically[F] {
        (for {
          wins2 <- disp.getWindows(focus)
        } yield if (wins == wins2) STM.retry[Set[Window]] else STM.pure(wins2)).flatten
      }
      _ <- loop(next)
    } yield ()

    for {
      wins <- atomically(disp.getWindows(focus))
      _    <- loop(wins)
    } yield ()
  }
}
