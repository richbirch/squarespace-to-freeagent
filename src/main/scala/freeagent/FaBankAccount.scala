package freeagent

case class FaBankAccount(url: Option[String],
                         opening_balance: Double,
                         bank_name: String,
                         name: String,
                         is_personal: Boolean,
                         is_primary: Boolean,
                         status: String,
                         currency: String,
                         current_balance: Double,
                         account_number: String,
                         sort_code: String,
                         bank_code: String,
                         latest_activity_date: String,
                         updated_at: String,
                         created_at: String
                        )
