package catz.datamanipulation

import cats.data.Reader

object ReaderExercise1 {
  case class AppConfig(dbConnectionString: String, apiKey: String)

  type AppReader[A] = Reader[AppConfig, A]

  def fetchFromDatabase(query: String): AppReader[List[String]] = Reader { config =>
    // Use config.dbConnectionString to connect to the database and fetch results
    List("some", "database", "results") // mock result
  }

  def callExternalAPI(endpoint: String): AppReader[String] = Reader { config =>
    // Use config.apiKey to make an API call
    "some API response" // mock response
  }

  def doBusinessLogic(): AppReader[String] = for {
    dbResults <- fetchFromDatabase("SELECT * FROM users")
    apiResponse <- callExternalAPI("/info")
  } yield s"Results: ${dbResults.mkString(", ")} and API Response: $apiResponse"

  def api= {
    val dbComputation: AppReader[List[String]] =fetchFromDatabase(" SELECT * FROM users ")
    val restEndPoint: AppReader[String] =  callExternalAPI("/info")
    dbComputation.flatMap(dbResults=>
      restEndPoint.map(apiResponse=> s"Results: ${dbResults.mkString(", ")} and API Response: $apiResponse")
    )
  }

  val appConfig = AppConfig("jdbc://localhost:5432/mydb", "SECRET_API_KEY")
  val result = doBusinessLogic().run(appConfig)
  println(result)  // Output: Results: some, database, results and API Response: some API response

}
