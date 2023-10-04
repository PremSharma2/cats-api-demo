package catz

object SemiGroupUseCase {


  import cats.Semigroup
  import cats.implicits._


  /*
TODO
 class MapMonoid[K, V](implicit V: Semigroup[V]) extends Monoid[Map[K, V]] {

  def empty: Map[K, V] = Map.empty

  def combine(xs: Map[K, V], ys: Map[K, V]): Map[K, V] =
    if (xs.size <= ys.size) {
      xs.foldLeft(ys) {
        case (my, (k, x)) =>
          my.updated(k, Semigroup.maybeCombine(x, my.get(k)))
      }
    } else {
      ys.foldLeft(xs) {
        case (mx, (k, y)) =>
          mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
      }
    }

   */
  // Define a class for items in the store
  case class ShoppingCart(items: Map[String, Int])

  // Define a Semigroup instance for ShoppingCart to combine carts

  /*
  TODO
       @inline def instance[A](cmb: (A, A) => A): Semigroup[A] = new Semigroup[A] {
    override def combine(x: A, y: A): A = cmb(x, y)
  }
   */
  implicit val shoppingCartSemigroup: Semigroup[ShoppingCart] =
    Semigroup.instance((cart1, cart2) => ShoppingCart(cart1.items.combine(cart2.items)))

  def main(args: Array[String]): Unit = {
    // Sample shopping carts
    val cart1 = ShoppingCart(Map.apply("item1" -> 2, "item2" -> 3))
    val cart2 = ShoppingCart(Map.apply("item2" -> 1, "item3" -> 4))

    // Combine shopping carts using Semigroup's |+| operator
    val combinedCart = cart1 |+| cart2

    // Print the result
    println(s"Combined Cart: ${combinedCart.items}")
  }



}
