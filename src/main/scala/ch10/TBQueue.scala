package ch10

import io.github.timwspence.cats.stm.{STM, TVar}
import cats.syntax.flatMap._

case class TBQueue[A](cap: TVar[Int], readVar: TVar[List[A]], writeVar: TVar[List[A]]) {
  def write(a: A): STM[Unit] = for {
    avail   <- cap.get
    _       <- if (avail == 0) STM.retry else cap.set(avail - 1)
    listend <- writeVar.get
    _       <- writeVar.set(a :: listend)
  } yield ()

  def read: STM[A] = (for {
    avail <- cap.get
    _     <- cap.set(avail + 1)
    xs    <- readVar.get
  } yield xs) flatMap {
    case x :: xs => readVar.set(xs) >> STM.pure(x)
    case Nil     => writeVar.get.flatMap {
      case Nil => STM.retry
      case ys  => for {
        _       <- writeVar.set(List.empty[A])
        z :: zs =  ys.reverse
        _       <- readVar.set(zs)
      } yield z
    }
  }
}

object TBQueue {
  def apply[A](size: Int): STM[TBQueue[A]] = for {
    read  <- TVar.of(List.empty[A])
    write <- TVar.of(List.empty[A])
    cap   <- TVar.of(size)
  } yield TBQueue(cap, read, write)
}