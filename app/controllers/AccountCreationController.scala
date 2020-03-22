package controllers

import authentication.AuthenticationAction
import javax.inject.Inject
import models.JsonFormats._
import models.{LoginDetails, Person, Search, UpdatePassword}
import play.api.libs.json.Json
import play.api.mvc._
import play.modules.reactivemongo.{MongoController, ReactiveMongoApi, ReactiveMongoComponents}
import reactivemongo.api.Cursor
import reactivemongo.play.json._
import reactivemongo.play.json.collection.{JSONCollection, _}
import scala.concurrent.{ExecutionContext, Future}

class AccountCreationController @Inject()(
                                           components: ControllerComponents, authAction: AuthenticationAction,
                                           val reactiveMongoApi: ReactiveMongoApi
                                         ) extends AbstractController(components)
  with MongoController with ReactiveMongoComponents with play.api.i18n.I18nSupport {

  implicit def ec: ExecutionContext = components.executionContext

  def collection: Future[JSONCollection] = database.map(_.collection[JSONCollection]("persons"))


  def create: Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    Person.accountCreation.bindFromRequest.fold({ formWithErrors =>
      Future.successful(BadRequest(views.html.signup(formWithErrors)))
    }, { person =>

      val cursor: Future[Cursor[Person]] = collection.map {
        _.find(Json.obj("username" -> person.username)).
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

      futureUsersList.map { value =>
        if (value.headOption.isEmpty) {
          collection.flatMap(_.insert.one(person)).map { _ => Ok(request2Messages.messages("accountCreated"))
          }
          Ok(request2Messages.messages("accountCreated"))
        }
        else BadRequest(request2Messages.messages("usernameInUser"))
      }
    })
  }


  def delete: Action[AnyContent] = authAction { implicit request: Request[AnyContent] =>
    Ok(views.html.delete(LoginDetails.loginForm))
  }


  def deleteSubmit: Action[AnyContent] = authAction.async { implicit request: Request[AnyContent] =>
    Person.accountDeletion.bindFromRequest.fold({ formWithErrors =>
      Future.successful(BadRequest(views.html.delete(formWithErrors)))
    }, { el =>

      val cursor: Future[Cursor[Person]] = collection.map {
        _.find(Json.obj("username" -> el.username, "password" -> el.password)).
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

      futureUsersList.map { value =>
        if (value.headOption.nonEmpty) {
          collection.flatMap(_.remove(value.head)).map { _ => Ok(request2Messages.messages("accountDeleted"))
          }
          Ok(request2Messages.messages("accountDeleted"))
        }
        else BadRequest(request2Messages.messages("accountNotFound"))
      }
    })
  }


  def findByUsername: Action[AnyContent] = authAction { implicit request: Request[AnyContent] =>
    Ok(views.html.search(Search.accountSearchUsername))
  }


  def updatePassword: Action[AnyContent] = authAction { implicit request: Request[AnyContent] =>
    Ok(views.html.update(UpdatePassword.accountUpdatePassword))
  }


  def updatePasswordSubmit: Action[AnyContent] = authAction.async { implicit request: Request[AnyContent] =>
    UpdatePassword.accountUpdatePassword.bindFromRequest.fold({ formWithErrors =>
      Future.successful(BadRequest(views.html.update(formWithErrors)))
    }, { el =>

      val cursor: Future[Cursor[Person]] = collection.map {
        _.find(Json.obj("username" -> el.username, "password" -> el.password)).
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

      futureUsersList.map { value =>
        val head = value.headOption
        if (head.nonEmpty) {
          val person = head.get
          val update = Person(person._id, person.name, person.age, person.username, el.UpdatePassword)
          collection.flatMap(_.update(person, update).map { _ => Ok(request2Messages.messages("accountUpdated")) })
          Ok(request2Messages.messages("accountUpdated"))
        }
        else {
          BadRequest(request2Messages.messages("accountNotFound"))
        }
      }

    })
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
