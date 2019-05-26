package ch07

import cats.effect._
import cats.effect.concurrent.MVar

object MVar1IOMain extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = for {
    m <- MVar.empty[IO, Char] // 空の MVar を作る
    _ <- m.put('x').start     // フォークした別スレッドで 'x' を MVar に入れる
    r <- m.take               // MVar から値を取り出して
    _ <- IO { println(r) }    // 出力する
  } yield ExitCode.Success
}
