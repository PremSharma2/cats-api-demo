package catz

import scala.language.higherKinds

object MonadExercise1 {
  import cats.Monad
  import cats.implicits._

  import cats.Monad
  import cats.implicits._
/*
TODO
    getCart retrieves the current cart.
    It returns an error if the cart is empty.
    addItem :
    adds an item to the cart.
    removeItem:
    removes an item from the cart by its ID.
    It returns an error if the item is not found.
    getTotalPrice:
    calculates the total price of all items in the cart.
    It returns an error if the cart is empty.
 TODO
   By using a monad, we've made the implementation of ShoppingCartAPI very flexible.
   It can work with any type that has a Monad instance,
   allowing for various kinds of effects
   (like Option, Either, or even types that can model asynchronous computations or handle exceptions, like Future).
 */
  import cats.Monad
  import cats.implicits._

  // Models
  case class Item(id: String, name: String, price: Double)
  case class Cart(items: List[Item])

  // Errors Adts
  sealed trait CartError
  case object ItemNotFound extends CartError
  case object CartIsEmpty extends CartError

  // Cart API trait
  trait ShoppingCartAPI[F[_]] {
    def getCart: F[Either[CartError, Cart]]
    def addItem(item: Item): F[Either[CartError, Cart]]
    def removeItem(itemId: String): F[Either[CartError, Cart]]
    def getTotalPrice: F[Either[CartError, Double]]
  }

  // Implementation
  class ShoppingCartService[F[_]: Monad] extends ShoppingCartAPI[F] {

    private var cart: Cart = Cart(Nil)

    override def getCart: F[Either[CartError, Cart]] =
      if (cart.items.isEmpty) Monad[F].pure(Left(CartIsEmpty)) else Monad[F].pure(Right(cart))

    override def addItem(item: Item): F[Either[CartError, Cart]] = {
      cart = Cart(item :: cart.items)
      Monad[F].pure(Right(cart))
    }

    override def removeItem(itemId: String): F[Either[CartError, Cart]] = {
      cart.items.find(_.id == itemId) match {
        case Some(itemToRemove) =>
          cart = Cart(cart.items.filterNot(_ == itemToRemove))
          Monad[F].pure(Right(cart))
        case None =>
          Monad[F].pure(Left(ItemNotFound))
      }
    }

    override def getTotalPrice: F[Either[CartError, Double]] =
      if (cart.items.isEmpty) Monad[F].pure(Left(CartIsEmpty)) else Monad[F].pure(Right(cart.items.map(_.price).sum))
  }

  // Usage Example
  def main(args: Array[String]): Unit = {


    val shoppingCartService = new ShoppingCartService[Option]

    // Add items to cart and get updated cart
    println(shoppingCartService.addItem(Item("1", "Apple", 0.99)))
    println(shoppingCartService.addItem(Item("2", "Banana", 1.10)))

    // Get total price
    println(shoppingCartService.getTotalPrice)

    // Remove item from cart and get updated cart
    println(shoppingCartService.removeItem("1"))

    // Get cart
    println(shoppingCartService.getCart)
  }

}
