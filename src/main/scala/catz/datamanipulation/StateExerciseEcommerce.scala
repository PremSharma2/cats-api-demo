package catz.datamanipulation

object StateExerciseEcommerce {

  /**
TODO
     Imagine you're implementing an e-commerce website.
     Users can browse products, add them to their cart,
     remove them, and ultimately check out to make a purchase.
     State
    The state for our shopping cart might be defined as:

   */
  type ProductName= String
  type Quantity= Int
  type Total= Double
  type Price= Double
  type ProductId= Int

  case class Product(productId: ProductId, name: ProductName, price: Price)

  case class CartItem(product: Product, quantity: Quantity)

  case class ShoppingCart(items: List[CartItem], total: Total)

  /**
   TODO  Basic Actions

TODO
  Add Product to Cart:
 Users can add products to their cart.
   If the product is already in the cart, its quantity increases.

TODO
 Remove Product from Cart:
 Users can remove products from their cart.
   This action might decrease the quantity
   or remove the item altogether if the quantity becomes zero.

TODO
  Calculate Total:
  Each time an item is added or removed,
   the total price in the cart should be recalculated.
TODO
 Using the State monad in Cats, these actions can be modeled as:
   */
  import cats.data.State

  def addToCart(product: Product, quantity: Quantity): State[ShoppingCart, String] = State { cart =>
    val existingItem = cart.items.find(_.product.productId == product.productId)

    val updatedItems = existingItem match {
      case Some(item) =>
        cart.items.map(i => if(i.product.productId == product.productId) i.copy(quantity = i.quantity + quantity) else i)
      case None =>
        cart.items :+ CartItem(product, quantity)
    }

    val updatedTotal = updatedItems.map(item => item.product.price * item.quantity).sum
    (ShoppingCart(updatedItems, updatedTotal), s"Added $quantity of ${product.name} to cart.")
  }

  def removeFromCart(productId: ProductId): State[ShoppingCart, String] = State {
    cart =>
    val updatedItems = cart.items.filterNot(_.product.productId == productId)
    val updatedTotal = updatedItems.map(item => item.product.price * item.quantity).sum
    (ShoppingCart(updatedItems, updatedTotal), s"Removed product $productId from cart.")
  }

  // Sample usage
  val product1 = Product(1, "Book", 20.0)
  val product2 = Product(2, "Shirt", 30.0)

  val initialState = ShoppingCart(List.empty, 0.0)

  val result = for {
    _ <- addToCart(product1, 2)
    _ <- addToCart(product2, 1)
    r <- removeFromCart(1)
  } yield r

  println(result.run(initialState).value._2)  // Expected: Removed product 1 from cart.

}
