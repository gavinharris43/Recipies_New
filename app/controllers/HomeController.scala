package controllers

import authentication.AuthenticationAction
import javax.inject._
import play.api.i18n.I18nSupport
import play.api.mvc._
import service.MongoService

import scala.concurrent.ExecutionContext.Implicits.global

@Singleton
class HomeController @Inject()(cc: ControllerComponents, authAction: AuthenticationAction, val mongoService: MongoService) extends AbstractController(cc) with I18nSupport {

  def index: Action[AnyContent] = authAction { implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }

  def showRecords(): Action[AnyContent] = Action.async {
    mongoService.findAll().map(listOfUsers =>
      Ok(listOfUsers.toString())
    )
  }

}
