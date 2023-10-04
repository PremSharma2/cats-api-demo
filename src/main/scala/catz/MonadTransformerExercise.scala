package catz

object MonadTransformerExercise  extends App {
  import cats.data.{EitherT, OptionT}
  import cats.implicits._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future

  case class Product(id: Int, name: String, price: Double)
  case class Cart(items: List[Product])
  case class Error(message: String)

  type FutureEither[A] = EitherT[Future, Error, A]
  def findProductById(id: Int): Future[Option[Product]] = Future {
    Option(Product(id, s"Product-$id", id * 10.0)).filter(_ => id != 0) // id 0 is not found
  }

  def addToCart(cart: Cart, product: Product): Future[Either[Error, Cart]] = Future {
    if (product.price > 100) Left(Error("Product too expensive"))
    else Right(cart.copy(items = product :: cart.items))
  }
/*
TODO
     OptionT.toRight to convert the failure case of the OptionT
     into an Either[Error, Product] with an appropriate error message.
     Now the EitherT can handle both kinds of errors:
     those coming from OptionT and those coming from addToCart.
     As a result, operation.value
     will return a Future[Either[Error, Cart]], as expected.
     When OptionT.toRight is called,
     it performs a transformation on the underlying F[Option[A]]
     to make it into F[Either[B, A]]. The transformation looks something
     // If the Option inside Future is Some, convert it to Right
    // If the Option inside Future is None, convert it to Left with the provided error
   futureOption.map(option => option.toRight(leftValueIfNone))
   You can now combine this EitherT with other EitherTs in a for-comprehension,
   thus simplifying the code and making it easier to follow.
   else there will be two different monad and for comprehension will not work
   this way

 */
  def addToCartIfAvailable(productId: Int, cart: Cart): Future[Either[Error, Cart]] = {
    val operation = for {
      productOption <- OptionT(findProductById(productId)).toRight(Error("Product not found"))
      updatedCart <- EitherT(addToCart(cart, productOption))
    } yield updatedCart

    operation.value
  }

  override def main(args: Array[String]): Unit = {
    // "Tests" using println
    val initialCart = Cart(List())

    // Test 1: Adding an available and affordable product
    addToCartIfAvailable(1, initialCart).foreach {
      case Left(error) => println(s"Test 1 Failed: ${error.message}")
      case Right(cart) => println(s"Test 1 Passed: ${cart.items.map(_.name)}")
    }

    // Test 2: Adding an expensive product
    addToCartIfAvailable(11, initialCart).foreach {
      case Left(error) => println(s"Test 2 Passed: ${error.message}")
      case Right(cart) => println(s"Test 2 Failed: ${cart.items.map(_.name)}")
    }

    // Test 3: Adding a non-existent product
    addToCartIfAvailable(0, initialCart).foreach {
      case Left(error) => println(s"Test 3 Passed: ${error.message}")
      case Right(cart) => println(s"Test 3 Failed: ${cart.items.map(_.name)}")
    }

    // Keep the JVM alive to let the Future computations finish
    Thread.sleep(2000)
  }

}


