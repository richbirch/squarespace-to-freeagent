import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import org.slf4j.{Logger, LoggerFactory}
import org.specs2.mutable.SpecificationLike

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.util.Try

case class RootContact(contact: FaContact)

case class FaContact(id: Option[Int],
                     url: Option[String],
                     first_name: String,
                     last_name: String,
                     organisation_name: Option[String],
                     email: String,
                     phone_number: Option[String],
                     address1: String,
                     address2: Option[String],
                     address3: Option[String],
                     town: String,
                     region: String,
                     postcode: String,
                     country: String,
                     created_at: Option[String]) {
  lazy val idFromUrl: Option[Int] = url.flatMap(_.split("/").reverse.headOption.map(_.toInt))
}

object FaContact {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def apply(orderLine: List[String]): FaContact = {
    val (firstName, lastName) = orderLine(24).split(" ").toList match {
      case first :: last :: Nil => (first, last)
      case first :: Nil => (first, "")
      case _ => ("", "")
    }

    FaContact(None, None, firstName, lastName, None, orderLine(1), Option(orderLine(31)), orderLine(25), Option(orderLine(26)), None, orderLine(27), orderLine(29), orderLine(28), orderLine(30), None)
  }
}

case class FaContactService(token: String, faHost: String) {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def updateOrCreate(contact: FaContact): Option[FaContact] = {
    findContact(token, contact) match {
      case None =>
        log.info(s"Contact not found. Creating")
        createContact(token, contact)
      case Some(existingContact) =>
        log.info(s"Contact found. Updating")
        updateContact(token, contact.copy(id = existingContact.idFromUrl, url = existingContact.url))
    }
  }

  def findContact(token: String, contact: FaContact): Option[FaContact] = {
    val uri = Uri.from(scheme = "https", host = faHost, path = "/v2/contacts")

    val body: String = FaService.get(token, uri)

    val allContacts = decode[Map[String, Seq[FaContact]]](body) match {
      case Left(_) => Seq[FaContact]()
      case Right(contacts) => contacts.values.flatten
    }

    allContacts.find(c => c.email == contact.email && c.first_name == contact.first_name && c.last_name == contact.last_name)
  }

  def createContact(token: String, contact: FaContact): Option[FaContact] = {
    val uri = Uri.from(scheme = "https", host = faHost, path = "/v2/contacts")

    val contactAsJsonString = RootContact(contact).asJson.toString()

    val body: String = FaService.post(token, uri, contactAsJsonString)

    decode[Map[String, FaContact]](body) match {
      case Left(_) => None
      case Right(contacts) => contacts.values.headOption
    }
  }

  def updateContact(token: String, contact: FaContact): Option[FaContact] = {
    contact.id.flatMap(id => {
      val uri = Uri.from(scheme = "https", host = faHost, path = s"/v2/contacts/$id")

      val contactAsJsonString = RootContact(contact).asJson.toString()

      val body: String = FaService.put(token, uri, contactAsJsonString)

      decode[Map[String, FaContact]](body) match {
        case Left(_) => None
        case Right(contacts) => contacts.values.headOption
      }
    })
  }

  def deleteContact(token: String, contact: FaContact): Option[Int] = {
    contact.id.map(id => {
      val uri = Uri.from(scheme = "https", host = faHost, path = s"/v2/contacts/$id")
      FaService.delete(token, uri)
    })
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

case class RootInvoice(invoice: FaInvoice)

case class FaInvoiceItem(url: Option[String], description: String, quantity: Double, price: Double)

object FaInvoiceItem {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def apply(allItems: List[List[String]]): List[FaInvoiceItem] = {
    val items = allItems
      .map(itemLine => {
        val itemFields = itemLine.toIndexedSeq
        FaInvoiceItem(None, itemFields(17), Try(itemFields(16).toDouble).getOrElse(0d), Try(itemFields(18).toDouble).getOrElse(0d))
      })
    items
  }
}

case class FaInvoice(id: Option[Int],
                     reference: String,
                     payment_terms_in_days: Int = 0,
                     total_value: Double,
                     paid_value: Double,
                     bank_account: String,
                     dated_on: Option[String],
                     currency: String = "GBP",
                     url: Option[String],
                     created_at: Option[String] = None,
                     paid_on: Option[String],
                     contact: String,
                     invoice_items: Option[Seq[FaInvoiceItem]] = None
                    ) {
  lazy val idFromUrl: Option[Int] = url.flatMap(_.split("/").reverse.headOption.map(_.toInt))
}

object FaInvoice {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def apply(allLines: List[List[String]], newContact: FaContact, bankUrl: String): Option[FaInvoice] = {
    allLines match {
      case Nil =>
        log.warn(s"No lines to create invoice from")
        None
      case orderLine :: _ =>
        val items = FaInvoiceItem(allLines)

        val invoice = FaInvoice(
          id = None,
          reference = orderLine(0),
          total_value = Try(orderLine(11).toDouble).getOrElse(0d),
          paid_value = Try(orderLine(11).toDouble).getOrElse(0d),
          bank_account = bankUrl,
          dated_on = Option(orderLine(3)),
          url = None,
          paid_on = Option(orderLine(3)),
          contact = newContact.url.getOrElse(""),
          invoice_items = Option(items)
        )

        Option(invoice)
    }
  }
}

case class FaInvoiceService(token: String, faHost: String) {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def findInvoice(token: String, invoice: FaInvoice): Option[FaInvoice] = {
    val uri = Uri.from(scheme = "https", host = faHost, path = "/v2/invoices", queryString = Option("nested_invoice_items=true"))

    val body = FaService.get(token, uri)

    val allInvoices = decode[Map[String, Seq[FaInvoice]]](body) match {
      case Left(_) => Seq[FaInvoice]()
      case Right(invoices) =>
        log.info(s"invoices: $invoices")
        invoices.values.flatten
    }

    log.info(s"all invoices: $allInvoices")

    allInvoices.find(i => i.reference == invoice.reference)
  }

  def createInvoice(token: String, invoice: FaInvoice): Option[FaInvoice] = {
    val uri = Uri.from(scheme = "https", host = faHost, path = "/v2/invoices")

    val invoiceAsJsonString = RootInvoice(invoice).asJson.toString()

    val body = FaService.post(token, uri, invoiceAsJsonString)

    decode[Map[String, FaInvoice]](body) match {
      case Left(_) => None
      case Right(invoices) => invoices.values.headOption
    }
  }

  def updateInvoice(token: String, invoice: FaInvoice): Option[FaInvoice] = {
    invoice.id.flatMap(id => {
      val uri = Uri.from(scheme = "https", host = faHost, path = s"/v2/invoices/$id")

      val invoiceAsJsonString = RootInvoice(invoice).asJson.toString()

      val body = FaService.put(token, uri, invoiceAsJsonString)

      decode[Map[String, FaInvoice]](body) match {
        case Left(_) => None
        case Right(invoices) => invoices.values.headOption
      }
    })
  }

  def updateOrCreate(invoice: FaInvoice): Option[FaInvoice] = {
    findInvoice(token, invoice) match {
      case None =>
        log.info(s"Invoice not found. Creating")
        createInvoice(token, invoice)
      case Some(existingInvoice) =>
        log.info(s"Invoice found. Updating")
        updateInvoice(token, invoice.copy(id = existingInvoice.idFromUrl, url = existingInvoice.url))
    }
  }
}

class ConvertSsToFaSpec extends SpecificationLike {
  val config: Config = ConfigFactory.load
  val clientId: String = config.getString("freeagent.id")
  val secret: String = config.getString("freeagent.secret")
  val token: String = config.getString("freeagent.access_token")
  val faHost = "api.sandbox.freeagent.com"

  "Given a squarespace order line " +
    "When I ask for a freeagent json string " +
    "Then I see the proper json represenation of the squarespace order" >> {
    //    skipped("for now")

    val contactService = FaContactService(token, faHost)
    val invoiceService = FaInvoiceService(token, faHost)

    val csvContent =
      """"00001","someone@gmail.com","PAID","2018-01-01 09:28:00 +0000","pending","","GBP","36.00","4.99","0.00","0.00","40.99","","0.00","Delivery","2018-01-01 09:28:00 +0000","1","Handmade square basket bag","36.00","SQ1122334","","true","true","pending","John Doe","44 Some Road","","Freetown","F42 1AA","England","United Kingdom","01234567891","John Doe","44 Some Road","","Freetown","F42 1AA","England","United Kingdom","01234567891","","","Stripe","xx_aabbccddeeffgghhiijjkkll"
        |"00001","","","","","","","","","","","","","","","","1","Handmade tiny pastel bowls","18.00","SQ6366224","mint green outside/ speckled light grey inside","true","true","FULFILLED","","","","","","","","","","","","","","","","","","","",""
        |"00001","","","","","","","","","","","","","","","","1","Handmade tiny pastel bowls","18.00","SQ3900697","mint green outside & inside","true","true","FULFILLED","","","","","","","","","","","","","","","","","","","",""
      """.stripMargin
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

    orders.foreach {
      case Nil => println(s"No order lines to parse")
      case orderLine :: otherItems =>
        val contact: FaContact = FaContact(orderLine)
        println(s"Parsed contact $contact")
        val maybeContact: Option[FaContact] = contactService.updateOrCreate(contact)

        maybeContact.foreach(newContact => {
          val invoice: Option[FaInvoice] = FaInvoice(orderLine :: otherItems, newContact, "https://api.sandbox.freeagent.com/v2/bank_accounts/3710")
          invoice.foreach(invoiceService.updateOrCreate)
        })
    }

    true
  }

  "Given a freeagent access token " +
    "When I create a contact " +
    "Then I should see a response with the new contact's details in JSON" >> {
    skipped("for now")

    val contactService = FaContactService(token, faHost)

    val contact = FaContact(None, None, "John", "Doh", None, "someone@gmail.com", Option("01234567891"), "44 Some Road", None, None, "Freetown", "England", "F42 1AA", "United Kingdom", None /*ssDateToMillis("2018-01-01 09:28:00 +0000")*/)

    val maybeContact = contactService.createContact(token, contact)

    maybeContact.foreach(newContact => println(s"url: ${newContact.url}"))

    true
  }

  "Given some JSON string " +
    "When I ask for it as Json " +
    "Then I should be able to access its fields" >> {
    skipped("for now")
    val orderLine =
      """"00001","someone@gmail.com","PAID","2018-01-01 09:28:00 +0000","pending","","GBP","36.00","4.99","0.00","0.00","40.99","","0.00","Delivery","2018-01-01 09:28:00 +0000","1","Handmade square basket bag","36.00","SQ1122334","","true","true","pending","John Doe","44 Some Road","","Freetown","F42 1AA","England","United Kingdom","01234567891","John Doe","44 Some Road","","Freetown","F42 1AA","England","United Kingdom","01234567891","","","Stripe","xx_aabbccddeeffgghhiijjkkll""""
    val fields = orderLine.drop(1).dropRight(1).split("\",\"")

    val (firstName, lastName) = fields(24).split(" ").toList match {
      case first :: last :: Nil => (first, last)
      case first :: Nil => (first, "")
      case _ => ("", "")
    }
    val contact = FaContact(None, None, firstName, lastName, None, fields(1), Option(fields(31)), fields(25), Option(fields(26)), None, fields(27), fields(29), fields(28), fields(30), None /*ssDateToMillis(fields(15))*/)
    println(s"contact as json: ${contact.asJson}")

    val jsonString = """{"contact":{"url":"https://api.sandbox.freeagent.com/v2/contacts/44495","first_name":"John","last_name":"Doh","active_projects_count":0,"created_at":"2018-05-19T18:23:09.862Z","updated_at":"2018-05-19T18:23:09.862Z","email":"someone@gmail.com","phone_number":"01234567891","address1":"44 Some Road","town":"Freetown","region":"England","postcode":"F42 1AA","contact_name_on_invoices":true,"country":"United Kingdom","charge_sales_tax":"Auto","locale":"en","account_balance":"0.0","status":"Active","uses_contact_invoice_sequence":false}}"""
    val json = decode[Map[String, FaContact]](jsonString)
    println(s"json: $json")

    true
  }
}

//0"Order ID",
//1"Email",
//2"Financial Status",
//3"Paid at",
//4"Fulfillment Status",
//5"Fulfilled at",
//6"Currency",
//7"Subtotal",
//8"Shipping",
//9"Taxes",
//10"Amount Refunded",
//11"Total",
//12"Discount Code",
//13"Discount Amount",
//14"Shipping Method",
//15"Created at",
//16"Lineitem quantity",
//17"Lineitem name",
//18"Lineitem price",
//19"Lineitem sku",
//20"Lineitem variant",
//21"Lineitem requires shipping",
//22"Lineitem taxable",
//23"Lineitem fulfillment status",
//24"Billing Name",
//25"Billing Address1",
//26"Billing Address2",
//27"Billing City",
//28"Billing Zip",
//29"Billing Province",
//30"Billing Country",
//31"Billing Phone",
//32"Shipping Name",
//33"Shipping Address1",
//34"Shipping Address2",
//35"Shipping City",
//36"Shipping Zip",
//37"Shipping Province",
//38"Shipping Country",
//39"Shipping Phone",
//40"Cancelled at",
//41"Private Notes",
//42"Payment Method",
//43"Payment Reference"
