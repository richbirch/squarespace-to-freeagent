package freeagent

import com.typesafe.config.{Config, ConfigFactory}
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import org.specs2.mutable.SpecificationLike

class ConvertSsToFaSpec extends SpecificationLike {
  val config: Config = ConfigFactory.load
  val clientId: String = config.getString("freeagent.id")
  val secret: String = config.getString("freeagent.secret")
  val token: String = config.getString("freeagent.access_token")
  val faHost = "api.sandbox.freeagent.com"

  "Given the bank account api uri " +
    "When I ask for a list of bank accounts " +
    "Then I should get one bank account from the freeagent account" >> {

    val maybeAccount: Option[FaBankAccount] = FaBankAccountService(token, faHost).getBankAccount
    println(s"parsed account: $maybeAccount")

    true
  }

  "Given a squarespace order line " +
    "When I ask for a freeagent json string " +
    "Then I see the proper json represenation of the squarespace order" >> {
    //    skipped("for now")

    val csvContent =
      """"00001","someone@gmail.com","PAID","2018-01-01 09:28:00 +0000","pending","","GBP","36.00","4.99","0.00","0.00","40.99","","0.00","Delivery","2018-01-01 09:28:00 +0000","1","Handmade square basket bag","12.00","SQ1122334","","true","true","pending","John Doe","44 Some Road","","Freetown","F42 1AA","England","United Kingdom","01234567891","John Doe","44 Some Road","","Freetown","F42 1AA","England","United Kingdom","01234567891","","","Stripe","xx_aabbccddeeffgghhiijjkkll"
        |"00001","","","","","","","","","","","","","","","","1","Handmade tiny pastel bowls","12.00","SQ6366224","mint green outside/ speckled light grey inside","true","true","FULFILLED","","","","","","","","","","","","","","","","","","","",""
        |"00001","","","","","","","","","","","","","","","","1","Handmade tiny pastel bowls","12.00","SQ3900697","mint green outside & inside","true","true","FULFILLED","","","","","","","","","","","","","","","","","","","",""
      """.stripMargin

    val faService = FaService(token, faHost, csvContent)
    faService.persistOrders()

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
