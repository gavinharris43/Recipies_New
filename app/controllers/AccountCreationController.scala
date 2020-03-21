package controllers

import authentication.AuthenticationAction
import javax.inject.Inject
import models.JsonFormats._
import models.{LoginDetails, Person, Search}
import play.api.libs.json.Json
import play.api.mvc._
import play.modules.reactivemongo.{MongoController, ReactiveMongoApi, ReactiveMongoComponents}
import reactivemongo.api.Cursor
import reactivemongo.play.json._
import reactivemongo.play.json.collection.{JSONCollection, _}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class AccountCreationController @Inject()(
                                           components: ControllerComponents, authAction: AuthenticationAction,
                                           val reactiveMongoApi: ReactiveMongoApi
                                         ) extends AbstractController(components)
  with MongoController with ReactiveMongoComponents with play.api.i18n.I18nSupport {

  implicit def ec: ExecutionContext = components.executionContext

  def collection: Future[JSONCollection] = database.map(_.collection[JSONCollection]("persons"))


  // TODO:  implement uniqueness on username
  def create: Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    Person.accountCreation.bindFromRequest.fold({ formWithErrors =>
      Future.successful(BadRequest(views.html.signup(formWithErrors)))
    }, { person =>
      collection.flatMap(_.insert.one(person)).map { _ => Ok("User inserted")
      }
    })
  }

  //TODO : Improve
  def delete: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.delete(LoginDetails.loginForm))
  }

  def deleteSubmit: Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    Person.accountDeletion.bindFromRequest.fold({ formWithErrors =>
      Future.successful(BadRequest(views.html.delete(formWithErrors)))
    }, { el =>

      val cursor: Future[Cursor[Person]] = collection.map {
        _.find(Json.obj("username" -> el.username)).
          sort(Json.obj("created" -> -1)).
          cursor[Person]()
      }

      val futureUsersList: Future[List[Person]] =
        cursor.flatMap(
          _.collect[List](
            -1,
            Cursor.FailOnError[List[Person]]()
          )
        )

      futureUsersList.flatMap{ value =>

        collection.flatMap(_.remove(value.head)).map { _ => Ok("User Removed")
        }


      }
    })
  }

  def findByUsername: Action[AnyContent] = authAction { implicit request: Request[AnyContent] =>
    Ok(views.html.search(Search.accountSearchUsername))
  }


  def findByUsernameSubmit: Action[AnyContent] = authAction.async { implicit requesr: Request[AnyContent] =>
    Search.accountSearchUsername.bindFromRequest.fold({ formWithErrors =>
      Future.successful(BadRequest(views.html.search(formWithErrors)))
    }, { search =>

      val cursor: Future[Cursor[Person]] = collection.map {
        _.find(Json.obj("username" -> search.username)).
          sort(Json.obj("created" -> -1)).
          cursor[Person]()
      }

      val futureUsersList: Future[List[Person]] =
        cursor.flatMap(
          _.collect[List](
            -1,
            Cursor.FailOnError[List[Person]]()
          )
        )

      futureUsersList.map {
        persons =>
          Ok(persons.toString)
      }
    })
  }


  //def Update

  def findByName(name: String): Action[AnyContent] = Action.async {
    val cursor: Future[Cursor[Person]] = collection.map {
      _.find(Json.obj("name" -> name)).
        sort(Json.obj("created" -> -1)).
        cursor[Person]()
    }

    val futureUsersList: Future[List[Person]] =
      cursor.flatMap(
        _.collect[List](
          -1,
          Cursor.FailOnError[List[Person]]()
        )
      )

    futureUsersList.map {
      persons =>
        Ok(persons.toString)
    }
  }


  def signup() = Action {
    implicit request: Request[AnyContent] =>
      Ok(views.html.signup(Person.accountCreation))

  }

}
