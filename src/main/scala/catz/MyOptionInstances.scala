package catz

import catz.FunctorTypeClass.MyFunctor

trait MyOptionInstances {

  implicit val mycatsStdInstancesForOption: MyFunctor[Option] = new MyFunctor[Option] {
    override def map[A, B](option: Option[A])(f: A => B): Option[B] =
      option.map(f)
  }
}
