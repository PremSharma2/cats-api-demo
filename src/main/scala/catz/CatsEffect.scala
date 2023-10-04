package catz




/*
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp}
object CatsEffect  extends IOApp.Simple {


  /*
 TODO
    Cats Effect’s core data structure is its main effect type, IO.
  IO[A] instances describe computations that (if finished) evaluate to a value of type A,
  and which can perform arbitrary side effects (e.g. printing things, acquiring/releasing resources, etc).
   IOs can take many shapes and sizes, but I’ll use the simple ones for the async stuff I’ll show you later:
     */

  def extension[A](io: IO[A]): IO[Unit] = {
    val rs: IO[Unit] = io.map { value =>
      println(s"[${Thread.currentThread().getName}] $value")
      value
    }
    rs
  }
  val meaningOfLife: IO[Int] = IO(42)
  val favLang = IO("Scala")

  def run: IO[Unit] = `extension`(meaningOfLife)
  /*
 TODO
      Also for ease of demonstrating asynchronicity,
       I’ll decorate the IO type with an extension method
       which also prints the current thread and the value it’s about to compute:
   */
  val program: IO[Unit] =
    for {
      _ <- meaningOfLife
      _ <- favLang
    } yield ()

  program.unsafeRunSync()
  //=> hey!
  //=> hey!
  ()
}

 */
