package catz

object MonoidExercise {

  import cats.Monoid
  import cats.instances.int._ // For Monoid[Int]
  import cats.instances.map._ // For Monoid[Map[K, V]]
  import cats.syntax.semigroup._ // For |+| syntax



  // Define a case class to represent a shopping cart
  case class ShoppingCart(items: Map[String, Int])

  // Create a Monoid instance for ShoppingCart
  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(
    ShoppingCart(Map.empty),
    (cart1, cart2) => ShoppingCart(cart1.items.combine(cart2.items))
  )

  // Sample shopping carts
  val cart1 = ShoppingCart(Map("item1" -> 2, "item2" -> 3))
  val cart2 = ShoppingCart(Map("item2" -> 1, "item3" -> 4))
  val cart3 = ShoppingCart(Map("item1" -> 1, "item4" -> 2))

  // Combine shopping carts using Monoid's |+| operator
  val combinedCart = cart1 |+| cart2 |+| cart3

  // Resulting combined cart contains the merged items
  println("Combined Cart:")
  combinedCart.items.foreach { case (item, quantity) =>
    println(s"$item: $quantity")
  }


}
