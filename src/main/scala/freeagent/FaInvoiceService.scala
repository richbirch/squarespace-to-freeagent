package freeagent

import akka.http.scaladsl.model.Uri
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import org.slf4j.{Logger, LoggerFactory}

case class FaInvoiceService(token: String, faHost: String) {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def findInvoice(token: String, invoice: FaInvoice): Option[FaInvoice] = {
    val uri = Uri.from(scheme = "https", host = faHost, path = "/v2/invoices", queryString = Option("nested_invoice_items=true"))

    val body = FaService.get(token, uri)
    log.info(s"invoices body: $body")

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

        val invoiceWithId = invoice.copy(id = existingInvoice.idFromUrl, url = existingInvoice.url)

        val existingItemsWithoutIds = existingInvoice.invoice_items.map(items => items.map(_.copy(url = None)))

        if (existingItemsWithoutIds != invoice.invoice_items) {
          log.info(s"Invoice items have changed. Removing before re-adding")
          log.info(s"Removing ${existingInvoice.invoice_items}")
          existingInvoice.invoice_items.foreach(items =>
            items.foreach(item => {
              deleteInvoiceItem(token, item)
            }))
          updateInvoice(token, invoiceWithId)
        } else {
          updateInvoice(token, invoiceWithId.copy(invoice_items = None))
        }

    }
  }

  def deleteInvoiceItem(token: String, item: FaInvoiceItem): Option[Int] = {
    item.idFromUrl.map(id => {
      val uri = Uri.from(scheme = "https", host = faHost, path = s"/v2/invoice_items/$id")
      log.info(s"Deleting invoice item ${item.url}")
      FaService.delete(token, uri)
    })
  }
}
