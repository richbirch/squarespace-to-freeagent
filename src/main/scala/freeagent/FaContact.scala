package freeagent

import org.slf4j.{Logger, LoggerFactory}

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

case class RootContact(contact: FaContact)
