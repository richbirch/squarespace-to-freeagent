import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import com.typesafe.config.{Config, ConfigFactory}
import io.circe
import io.circe.generic.auto._
import io.circe.syntax._
import org.joda.time.{DateTime, DateTimeZone}
import org.specs2.mutable.SpecificationLike
import io.circe.parser.decode

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.Try

case class RootContact(contact: FaContact)

case class FaContact(url: Option[String],
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
  lazy val id: Option[String] = url.flatMap(_.split("/").reverse.headOption)
}

case class FaContactReturned(url: Option[String],
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
                             created_at: String)

case class RootInvoice(invoice: FaInvoice)

case class FaInvoiceItem(description: String, item_type: String, quantity: Double, price: Double)

case class FaInvoice(reference: String,
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
                    )

//case class FaInvoiceLight(reference: String,
//                          total_value: Double,
//                          paid_value: Double,
//                          bank_account: String,
//                          dated_on: Option[String],
//                          currency: String = "GBP",
//                          url: Option[String],
//                          created_at: Option[String] = None,
//                          paid_on: Option[String],
//                          contact: String
//                         )

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

    val orderLine = """"00001","someone@gmail.com","PAID","2018-01-01 09:28:00 +0000","pending","","GBP","36.00","4.99","0.00","0.00","40.99","","0.00","Delivery","2018-01-01 09:28:00 +0000","1","Handmade square basket bag","36.00","SQ1122334","","true","true","pending","John Doe","44 Some Road","","Freetown","F42 1AA","England","United Kingdom","01234567891","John Doe","44 Some Road","","Freetown","F42 1AA","England","United Kingdom","01234567891","","","Stripe","xx_aabbccddeeffgghhiijjkkll""""
    val fields = orderLine.drop(1).dropRight(1).split("\",\"")

    val millis: Long = ssDateToMillis(fields(15))
    val (firstName, lastName) = fields(24).split(" ").toList match {
      case first :: last :: Nil => (first, last)
      case first :: Nil => (first, "")
      case _ => ("", "")
    }

    val contact = FaContact(None, firstName, lastName, None, fields(1), Option(fields(31)), fields(25), Option(fields(26)), None, fields(27), fields(29), fields(28), fields(30), None)
    val maybeContact = findContact(token, contact) match {
      case None =>
        println(s"Contact not found. Creating")
        createContact(token, contact)
      case someContact =>
        println(s"Contact found. Re-using")
        someContact
    }

    maybeContact.map(newContact => {
      val item = FaInvoiceItem("Nice stuff", "Products", 1, 35.99)
      val invoice = FaInvoice(
        reference = fields(0),
        total_value = Try(fields(11).toDouble).getOrElse(0d),
        paid_value = Try(fields(11).toDouble).getOrElse(0d),
        bank_account = "https://api.sandbox.freeagent.com/v2/bank_accounts/3710",
        dated_on = Option(fields(3)),
        url = None,
        paid_on = Option(fields(3)),
        contact = newContact.url.getOrElse(""),
        invoice_items = Option(Seq(item))
      )

      val updatedInvoice = findInvoice(token, invoice) match {
        case None =>
          println(s"Invoice not found. Creating")
          createInvoice(token, invoice)
        case someInvoice =>
          println(s"Invoice found. Re-using")
          someInvoice
      }

      println(s"$updatedInvoice")
    })

    true
  }

  "Given a freeagent access token " +
    "When I create a contact " +
    "Then I should see a response with the new contact's details in JSON" >> {
    skipped("for now")

    val contact = FaContact(None, "John", "Doh", None, "someone@gmail.com", Option("01234567891"), "44 Some Road", None, None, "Freetown", "England", "F42 1AA", "United Kingdom", None /*ssDateToMillis("2018-01-01 09:28:00 +0000")*/)

    val maybeContact = createContact(token, contact)

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

    val millis: Long = ssDateToMillis(fields(15))
    val (firstName, lastName) = fields(24).split(" ").toList match {
      case first :: last :: Nil => (first, last)
      case first :: Nil => (first, "")
      case _ => ("", "")
    }
    val contact = FaContact(None, firstName, lastName, None, fields(1), Option(fields(31)), fields(25), Option(fields(26)), None, fields(27), fields(29), fields(28), fields(30), None /*ssDateToMillis(fields(15))*/)
    println(s"contact as json: ${contact.asJson}")

    val jsonString = """{"contact":{"url":"https://api.sandbox.freeagent.com/v2/contacts/44495","first_name":"John","last_name":"Doh","active_projects_count":0,"created_at":"2018-05-19T18:23:09.862Z","updated_at":"2018-05-19T18:23:09.862Z","email":"someone@gmail.com","phone_number":"01234567891","address1":"44 Some Road","town":"Freetown","region":"England","postcode":"F42 1AA","contact_name_on_invoices":true,"country":"United Kingdom","charge_sales_tax":"Auto","locale":"en","account_balance":"0.0","status":"Active","uses_contact_invoice_sequence":false}}"""
    val json = decode[Map[String, FaContact]](jsonString)
    println(s"json: $json")

    true
  }

  def findContact(token: String, contact: FaContact): Option[FaContact] = {
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val uri = Uri.from(scheme = "https", host = faHost, path = "/v2/contacts")

    val request = HttpRequest(
      method = HttpMethods.GET,
      uri = uri
    ).withHeaders(
      RawHeader("Authorization", s"Bearer $token")
    )
    val responseFuture: Future[HttpResponse] = Http().singleRequest(request)

    val bodyFuture = responseFuture
      .map(httpResponse => {
        httpResponse.entity.dataBytes.runFold(ByteString(""))(_ ++ _).map { body =>
          body.utf8String
        }
      })
      .flatten

    val body = Await.result(bodyFuture, 10 seconds)

    val allContacts = decode[Map[String, Seq[FaContact]]](body) match {
      case Left(_) => Seq()
      case Right(contacts) => contacts.values.flatMap(identity)
    }

    allContacts.foreach(c => {
      deleteContact(token, c) match {
        case Some(status) => println(s"${c.url} $status")
        case None => println(s"${c.url} no status (failed?)")
      }
    })

    allContacts.find(c => c.email == contact.email && c.first_name == contact.first_name && c.last_name == contact.last_name)
  }

  def createContact(token: String, contact: FaContact): Option[FaContact] = {
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val uri = Uri.from(scheme = "https", host = faHost, path = "/v2/contacts")

    val contactAsJsonString = RootContact(contact).asJson.toString()
    println(s"posting contact:\n$contactAsJsonString\n\n")

    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = uri,
      entity = HttpEntity(ContentType(MediaTypes.`application/json`), contactAsJsonString))
      .withHeaders(
        RawHeader("Authorization", s"Bearer $token")
      )
    val responseFuture: Future[HttpResponse] = Http().singleRequest(request)

    val bodyFuture = responseFuture
      .map(httpResponse => {
        httpResponse.entity.dataBytes.runFold(ByteString(""))(_ ++ _).map { body =>
          body.utf8String
        }
      })
      .flatten

    val body = Await.result(bodyFuture, 10 seconds)

    decode[Map[String, FaContact]](body) match {
      case Left(_) => None
      case Right(contacts) => contacts.values.headOption
    }
  }

  def deleteContact(token: String, contact: FaContact): Option[Int] = {
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val result = contact.id.map(id => {
      val uri = Uri.from(scheme = "https", host = faHost, path = s"/v2/contacts/$id")

      val request = HttpRequest(
        method = HttpMethods.DELETE,
        uri = uri
      ).withHeaders(
        RawHeader("Authorization", s"Bearer $token")
      )
      val responseFuture: Future[HttpResponse] = Http().singleRequest(request)

      val status: Future[Int] = responseFuture.map(_.status.intValue)

      Await.result(status, 10 seconds)
    })

    result
  }

  def findInvoice(token: String, invoice: FaInvoice): Option[FaInvoice] = {
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val uri = Uri.from(scheme = "https", host = faHost, path = "/v2/invoices", queryString = Option("nested_invoice_items=true"))

    val request = HttpRequest(
      method = HttpMethods.GET,
      uri = uri
    ).withHeaders(
      RawHeader("Authorization", s"Bearer $token")
    )
    val responseFuture: Future[HttpResponse] = Http().singleRequest(request)

    val bodyFuture = responseFuture
      .map(httpResponse => {
        httpResponse.entity.dataBytes.runFold(ByteString(""))(_ ++ _).map { body =>
          println(s"body: ${body.utf8String}")
          body.utf8String
        }
      })
      .flatten

    val body = Await.result(bodyFuture, 10 seconds)

    val allInvoices = decode[Map[String, Seq[FaInvoice]]](body) match {
      case Left(_) => Seq()
      case Right(invoices) =>
        println(s"invoices: $invoices")
        invoices.values.flatMap(identity)
    }
    println(s"all invoices: $allInvoices")

    allInvoices.find(i => i.reference == invoice.reference)
  }

  def createInvoice(token: String, invoice: FaInvoice): Option[FaInvoice] = {
    implicit val system: ActorSystem = ActorSystem()
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val uri = Uri.from(scheme = "https", host = faHost, path = "/v2/invoices")

    val invoiceAsJsonString = RootInvoice(invoice).asJson.toString()
    println(s"posting invoice:\n$invoiceAsJsonString\n\n")

    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = uri,
      entity = HttpEntity(ContentType(MediaTypes.`application/json`), invoiceAsJsonString))
      .withHeaders(
        RawHeader("Authorization", s"Bearer $token")
      )
    val responseFuture: Future[HttpResponse] = Http().singleRequest(request)

    val bodyFuture = responseFuture
      .map(httpResponse => {
        httpResponse.entity.dataBytes.runFold(ByteString(""))(_ ++ _).map { body =>
          println(s"body: ${body.utf8String}")
          body.utf8String
        }
      })
      .flatten

    val body = Await.result(bodyFuture, 10 seconds)

    decode[Map[String, FaInvoice]](body) match {
      case Left(e) => None
      case Right(invoices) => invoices.values.headOption
    }
  }

  def ssDateToMillis(ssDate: String): Long = {
    val dateParts = ssDate.split(" ")
    val dateToParse = s"${dateParts(0)}T${dateParts(1)}+${dateParts(2).drop(1)}"
    val millis = new DateTime(dateToParse, DateTimeZone.UTC).getMillis
    millis
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
