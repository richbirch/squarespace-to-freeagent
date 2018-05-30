package freeagent

import akka.http.scaladsl.model.Uri
import io.circe.generic.auto._
import io.circe.parser.decode

case class FaBankAccountService(token: String, faHost: String) {
  def getBankAccount: Option[FaBankAccount] = {
    val uri = Uri.from(scheme = "https", host = faHost, path = "/v2/bank_accounts")

    val body = FaService.get(token, uri)

    decode[Map[String, Seq[FaBankAccount]]](body) match {
      case Left(_) => None
      case Right(accounts) =>
        accounts.values.flatMap(_.headOption).headOption
    }
  }
}
