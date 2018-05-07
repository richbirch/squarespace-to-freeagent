import io.circe.generic.auto._
import io.circe.syntax._
import org.joda.time.{DateTime, DateTimeZone}
import org.specs2.mutable.SpecificationLike

import scala.util.Try
import com.typesafe.config.ConfigFactory


case class FaContact(firstName: String,
                     lastName: String,
                     email: String,
                     phone: String,
                     mobile: String,
                     address1: String,
                     address2: String,
                     address3: String,
                     town: String,
                     region: String,
                     postcode: String,
                     country: String,
                     createdAt: Long)

case class FaInvoiceItem(description: String, quantity: Double, price: Double)

case class FaInvoice(contact: FaContact,
                     datedOn: Long,
                     totalValue: Double,
                     invoiceItems: Seq[FaInvoiceItem]) {
  val paymentTermsInDays = 0
}

//https://api.sandbox.freeagent.com/v2/approve_app
//https://api.sandbox.freeagent.com/v2/token_endpoint


class ConvertSsToFaSpec extends SpecificationLike {
  val config = ConfigFactory.load

  "Given a squarespace order line " +
    "When I ask for a freeagent json string " +
    "Then I see the proper json represenation of the squarespace order" >> {
    val token = config.getString("freeagent.access_token")

    println(s"token: $token")

    val orderLine = """"00001","someone@gmail.com","PAID","2018-01-01 09:28:00 +0000","pending","","GBP","36.00","4.99","0.00","0.00","40.99","","0.00","Delivery","2018-01-01 09:28:00 +0000","1","Handmade square basket bag","36.00","SQ1122334","","true","true","pending","John Doe","44 Some Road","","Freetown","F42 1AA","England","United Kingdom","01234567891","John Doe","44 Some Road","","Freetown","F42 1AA","England","United Kingdom","01234567891","","","Stripe","xx_aabbccddeeffgghhiijjkkll""""
    val fields = orderLine.drop(1).dropRight(1).split("\",\"")

    val millis: Long = ssDateToMillis(fields(15))
    val (firstName, lastName) = fields(24).split(" ").toList match {
      case first :: last :: Nil => (first, last)
      case first :: Nil => (first, "")
      case _ => ("", "")
    }
    
    val contact = FaContact(firstName, lastName, fields(1), fields(31), fields(31), fields(25), fields(26), "", fields(27), fields(29), fields(28), fields(30), ssDateToMillis(fields(15)))
    val invoice = FaInvoice(contact, millis, Try(fields(11).toDouble).getOrElse(0d), Seq())

    val invoiceJson = invoice.asJson

    val expectedContact = FaContact("John", "Doe", "someone@gmail.com", "01234567891", "01234567891", "44 Some Road", "", "", "Freetown", "England", "F42 1AA", "United Kingdom", ssDateToMillis("2018-01-01 09:28:00 +0000"))
    val expected = FaInvoice(expectedContact, new DateTime("2018-01-01T09:28Z").getMillis, 40.99, Seq()).asJson

    invoiceJson === expected
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
