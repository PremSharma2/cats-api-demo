package catz.datamanipulation

import cats.Id

object ReaderExercise3 {
  import cats.data.Reader

  //Data Models
  case class ShoppingCart(items: List[Item] = List.empty)
  case class Item(name: String, price: Double)

  // Define the configuration
  case class Config(discounts: Map[String, Double])

  // Function to add an item to the cart
  def addToCart(item: Item): Reader[ShoppingCart, ShoppingCart] =
//def apply[A, B](f: A => B): data.Reader[A, B]
    Reader(cart => ShoppingCart(item :: cart.items))

  // Function to calculate total price with discounts
  def calculateTotal: Reader[(Config, ShoppingCart), Double] =
    Reader {
      case (config, cart) =>
        cart.items.map { item =>
          config.discounts.get(item.name) match {
            case Some(discount) => item.price * (1 - discount)
            case None => item.price
          }
        }.sum
    }

  // Sample usage
  val config = Config(Map("apple" -> 0.1))  // 10% discount on apples
  val cart = ShoppingCart(List(Item("apple", 1.0), Item("orange", 0.5)))

  // Add a banana to the cart
  val updatedCart = addToCart(Item("banana", 0.2)).run(cart)

  // Calculate total price with discounts
  val total: Id[Double] = calculateTotal.run(config, updatedCart)

}
