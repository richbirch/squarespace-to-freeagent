package freeagent

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration._

case class FaService(token: String, faHost: String, csvContent: String) {
  val log: Logger = LoggerFactory.getLogger(getClass)

  val contactService = FaContactService(token, faHost)
  val invoiceService = FaInvoiceService(token, faHost)
  val bankAccountService = FaBankAccountService(token, faHost)

  def persistOrders(): Seq[FaInvoice] = {
    val maybeInvoices = csvOrders(csvContent)
      .map {
        case Nil =>
          log.info(s"No order lines to parse")
          None
        case orderLine :: otherItems =>
          val contact: FaContact = FaContact(orderLine)
          val maybeContact: Option[FaContact] = contactService.updateOrCreate(contact)
          val maybeBankAccount = bankAccountService.getBankAccount

          val maybeMaybeMaybeInvoice: Option[Option[Option[FaInvoice]]] = for {
            newContact <- maybeContact
            bankAccount <- maybeBankAccount
            accountUrl <- bankAccount.url
          } yield {
            val invoice: Option[FaInvoice] = FaInvoice(orderLine :: otherItems, newContact, accountUrl)
            invoice.map(invoiceService.updateOrCreate)
          }

          maybeMaybeMaybeInvoice.flatten.flatten
      }

    maybeInvoices.collect {
      case Some(order) => order
    }
  }

  def csvOrders(csvContent: String): List[List[List[String]]] = {
    val csvLines: List[String] = csvContent
      .split("\n")
      .toList

    val csvRows: List[List[String]] = csvLines
      .map(line => line
        .drop(1)
        .dropRight(1)
        .split("\",\"")
        .toList
      )
      .filter(fields => {
        fields.length == 44 || fields.length == 24
      })

    val orders = csvRows
      .groupBy(_.head)
      .values
      .toList
    orders
  }
}

object FaService {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def get(token: String, uri: Uri): String = {
    log.info(s"Getting @ $uri")
    val request = HttpRequest(
      method = HttpMethods.GET,
      uri = uri
    ).withHeaders(
      RawHeader("Authorization", s"Bearer $token")
    )

    getResponse(request)
  }

  def post(token: String, uri: Uri, jsonString: String): String = {
    log.info(s"Posting @ $uri: $jsonString")
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = uri,
      entity = HttpEntity(ContentType(MediaTypes.`application/json`), jsonString)
    ).withHeaders(
      RawHeader("Authorization", s"Bearer $token")
    )

    getResponse(request)
  }

  def put(token: String, uri: Uri, jsonString: String): String = {
    log.info(s"Putting @ $uri: $jsonString")
    val request = HttpRequest(
      method = HttpMethods.PUT,
      uri = uri,
      entity = HttpEntity(ContentType(MediaTypes.`application/json`), jsonString)
    ).withHeaders(
      RawHeader("Authorization", s"Bearer $token")
    )

    getResponse(request)
  }

  def delete(token: String, uri: Uri): Int = {
    log.info(s"Deleting @ $uri")
    val request = HttpRequest(
      method = HttpMethods.DELETE,
      uri = uri
    ).withHeaders(
      RawHeader("Authorization", s"Bearer $token")
    )

    getStatus(request)
  }

  def getResponse(request: HttpRequest): String = {
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val responseFuture: Future[HttpResponse] = Http().singleRequest(request)

    val bodyFuture = responseFuture
      .map(httpResponse => {
        httpResponse.entity.dataBytes.runFold(ByteString(""))(_ ++ _).map(_.utf8String)
      })
      .flatten

    Await.result(bodyFuture, 10 seconds)
  }

  def getStatus(request: HttpRequest): Int = {
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val responseFuture: Future[HttpResponse] = Http().singleRequest(request)

    val status: Future[Int] = responseFuture.map(_.status.intValue)

    Await.result(status, 10 seconds)
  }
}