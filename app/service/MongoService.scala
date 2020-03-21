package service

import javax.inject.{Inject, Singleton}
import models.JsonFormats._
import models.Person
import play.api.libs.json.Json
import play.modules.reactivemongo.{ReactiveMongoApi, ReactiveMongoComponents}
import reactivemongo.api.Cursor
import reactivemongo.api.commands.WriteResult
import reactivemongo.play.json._
import reactivemongo.play.json.collection.{JSONCollection, _}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class MongoService @Inject()(
                              val reactiveMongoApi: ReactiveMongoApi
                            ) extends ReactiveMongoComponents {


  def collection: Future[JSONCollection] = reactiveMongoApi.database.map(_.collection[JSONCollection]("persons"))

  def createUser(user: Person): Future[WriteResult] = {
    collection.flatMap(_.insert.one(user))
  }

  def findAll(): Future[List[Person]] = {
    collection.map {
      _.find(Json.obj())
        .sort(Json.obj("created" -> -1))
        .cursor[Person]()
    }.flatMap(el => {
      el.collect[List](
        -1,
        Cursor.FailOnError[List[Person]]()
      )
    }
    )
  }

}
