package catz.datamanipulation

import cats.Id
import cats.data.Kleisli

object ReaderExercise {
/*
TODO
 The first thing you need to know is that
 the Reader monad represents a function: A => B, i.e:
 class Reader[A,B](run: A => B) {
    // details omitted
 }
TODO
 We could use the Reader monad to achieve various things,
 but I would like to show you in this post how to use it to achieve composition and dependency injection.
 Composition
 Using the above class definition of the Reader
 we can create tow instances of a Reader with matching type.
 We have two Readers with matching type,
 we can combine them as we like to produce a new Reader without the risk of producing side effect.
 */
  import cats.data.Reader
  val upper = Reader((text: String) => text.toUpperCase)
  val greet = Reader((name: String) => s"Hello $name")
  val comb1 = upper.compose(greet)
  val comb2 = upper.andThen(greet)
  val result = comb1.run("Bob")
  println(result) // prints Hello Bob

  /*
TODO
 Dependency Injection
 Consider the two services below.
 The user can only register in a course if he is authorised.
 So the CourseService is dependent on the result of the AuthService
   */

  case class Course(desc: String, code: String)

  class AuthService {
    def isAuthorised(userName: String): Boolean = userName.startsWith("J")
  }

  class CourseService {
    def register(course: Course,
                 isAuthorised:
                 Boolean,
                 name: String) = {
      if (isAuthorised)
        s"User $name registered for the course: ${course.code}"
      else
        s"User: $name is not authorised to register for course: ${course.code}"
    }
  }
  /*
TODO
     Let’s create a class which represent the current environment and call it CourseManager.
   */
  case class CourseManager(course: Course,
                           userName: String,
                           authService: AuthService,
                           courseService: CourseService)

/*
TODO
      When the application is run the CourseManager
      will have all the services needed to carry out the business transaction.
      Let’s create the Reader monad which allows us to write business methods not tied to the services
 */
def isAuthorised = Reader[CourseManager, Boolean]{ courseMgr =>
  courseMgr.authService.isAuthorised(courseMgr.userName)
}

  def register(isFull: Boolean) = Reader[CourseManager, String] { courseMgr =>
    courseMgr.courseService.register(courseMgr.course,
      isFull,
      courseMgr.userName)
  }
  /*
TODO
     Note that in both functions, the CourseManager shows up in the return type.
     Therefore we can combine these two functions using for comprehension as follow:
   */
   val rs: Kleisli[Id, CourseManager, String] =isAuthorised.flatMap(flag => register(flag))
  val result1: Kleisli[Id, CourseManager, String] = for {
    authorised <- isAuthorised
    response <- register(authorised)
  } yield response

  def main(args: Array[String]): Unit = {
    val course = Course("Computer Science", "CS01")
    val report = result1.run(CourseManager(course, "Jon", new AuthService, new CourseService))
    println(report) // prints: User Jon registered for the course: CS01
  }
}
