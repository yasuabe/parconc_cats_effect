package ch07

import cats.effect.concurrent.MVar
import cats.effect._
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import eu.timepit.refined._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import parconc_util.InfoLogging._

object PhoneBookModule {
  type Name        = NonEmptyString
  type PhoneNumber = NonEmptyString
  type PhoneBook   = Map[Name, PhoneNumber]

  case class PhoneBookState[F[_]: Concurrent](m: MVar[F, PhoneBook]) {
    def insert(n: Name, p: PhoneNumber): F[Unit] =
      m.take >>= (book => m.put(book + (n -> p)))

    def lookup(n: Name): F[Option[PhoneNumber]] = for {
      book <- m.take
      _    <- m.put(book) // すぐ戻す
    } yield book.get(n)
  }
  object PhoneBookState {
    def apply[F[_]: Concurrent]: F[PhoneBookState[F]] =
      MVar.of(Map.empty[Name, PhoneNumber]).map(PhoneBookState(_))
  }
}
object PhoneBookMain extends IOApp {
  import ch07.PhoneBookModule._
  implicit def nonEmpty(s: String): NonEmptyString = { // for testing purpose
    assert(s.nonEmpty)
    refineV[NonEmpty](s).right.get
  }

  def program[F[_]](implicit F: Concurrent[F], C: Clock[F]) = for {
    s <- PhoneBookState[F]
    _ <- (1 to 10000).toList.map(n => s.insert(s"Name$n", s"$n")).sequence.void
    _ <- s.lookup("Name999") >>= info[F]
    _ <- s.lookup("Unknown") >>= info[F]
  } yield ()

  def run(args: List[String]): IO[ExitCode] =
    program[IO] as ExitCode.Success
}
