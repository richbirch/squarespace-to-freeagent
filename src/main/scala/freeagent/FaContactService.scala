package freeagent

import akka.http.scaladsl.model.Uri
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import org.slf4j.{Logger, LoggerFactory}

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
