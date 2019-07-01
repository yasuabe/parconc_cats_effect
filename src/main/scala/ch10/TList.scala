package ch10

import io.github.timwspence.cats.stm.{STM, TVar}
import cats.syntax.flatMap._

case class TList[A](tVars: TVar[List[A]]) {
  def write(a: A): STM[Unit] = for {
    list <- tVars.get
    _    <- tVars.set(list :+ a)
  } yield ()

  def read: STM[A] = tVars.get.flatMap {
    case Nil     => STM.retry
    case a :: as => tVars.set(as) >> STM.pure(a)
  }
}
object TList {
  def newTList[A]: STM[TList[A]] = TVar.of(List.empty[A]).map(TList(_))
}
