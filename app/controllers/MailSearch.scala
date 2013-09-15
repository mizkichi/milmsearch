package controllers

import models.search._
import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._
import javax.mail.internet.InternetAddress

object MailSearch extends Controller {

  val searchForm = Form(
    mapping(
      "keywords"   -> nonEmptyText,
      "fromDate"   -> jodaDate(Defaults.searchDateFormat),
      "toDate"     -> jodaDate(Defaults.searchDateFormat),
      "fields"     -> list(text),
      "mlIDs"      -> list(longNumber),
      "froms"      -> list(text),
      "order"      -> optional(text),
      "page"       -> optional(longNumber)
    ){
      (keywords, fromDate, toDate, fields, mlIDs, froms, order, page) =>
        SearchRequest(keywords, fromDate, toDate,
          fields map { MailSearchField.withName(_) } toSet,
          mlIDs toSet,
          froms map { FromOption.apply(_) } toSet,
          Defaults.ItemsPerPage,
          order.map(MailSearchOrder.withName(_)).getOrElse(MailSearchOrder.DateDesc),
          page.getOrElse(1L)
        )
    }{
      (search: SearchRequest) => Some(
        search.keywords,
        search.fromDate,
        search.toDate,
        search.fields map { _.toString } toList,
        search.mlIDs toList,
        search.froms map { _.value } toList,
        Some(search.order.toString),
        Some(search.page)
      )
    }
  )

  def search() = Action { implicit request =>

    searchForm.bindFromRequest.fold(
      errorForm => BadRequest(views.html.searchError(errorForm)),
      searchRequest => {
        Logger.debug("Search => " + searchRequest)
        val result = Searcher.search(searchRequest)

        // TODO dummy data -> real data
        Ok(views.html.searchResult(searchRequest, result._1,
          result._2,
          result._3)
        )
      }
    )
  }
}
