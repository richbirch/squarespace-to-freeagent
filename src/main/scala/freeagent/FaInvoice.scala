package freeagent

import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

case class RootInvoice(invoice: FaInvoice)

case class FaInvoiceItem(id: Option[Int],
                         url: Option[String],
                         description: String,
                         quantity: Double,
                         price: Double) {
  lazy val idFromUrl: Option[Int] = url.flatMap(_.split("/").reverse.headOption.map(_.toInt))
}

object FaInvoiceItem {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def apply(allItems: List[List[String]]): List[FaInvoiceItem] = {
    val items = allItems
      .map(itemLine => {
        val itemFields = itemLine.toIndexedSeq
        FaInvoiceItem(None, None, itemFields(17), Try(itemFields(16).toDouble).getOrElse(0d), Try(itemFields(18).toDouble).getOrElse(0d))
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
                     payment_methods: Map[String, String],
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
          payment_methods = Map("stripe" -> "true"),
          invoice_items = Option(items)
        )

        Option(invoice)
    }
  }
}