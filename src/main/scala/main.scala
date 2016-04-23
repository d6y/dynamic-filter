import slick.driver.H2Driver.api._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Example extends App {


  //
  // A simple one table database
  //

  final case class Message(
    sender:  String,
    content: Option[String],
    id:      Long = 0L)

  def freshTestData = Seq(
    Message("Dave", Some("Hello, HAL. Do you read me, HAL?")),
    Message("HAL",  Some("Affirmative, Dave. I read you.")),
    Message("Dave", Some("Open the pod bay doors, HAL.")),
    Message("HAL",  Some("I'm sorry, Dave. I'm afraid I can't do that.")),
    Message("Dave", None)
  )

  final class MessageTable(tag: Tag)
      extends Table[Message](tag, "message") {

    def id      = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def sender  = column[String]("sender")
    def content = column[Option[String]]("content")

    def * = (sender, content, id) <> (Message.tupled, Message.unapply)
  }

  lazy val messages = TableQuery[MessageTable]

  val db = Database.forConfig("example")

  def exec[T](program: DBIO[T]): T = Await.result(db.run(program), 2 seconds)

  exec(messages.schema.create)
  exec(messages ++= freshTestData)

  //
  // Now for some filters...
  //

  import Filters._

  val cmp: Comparison = EQ
  val value = 1L

  val query1 = cmp.filter(value, messages)(_.id)        // ok
  val query2 = LIKE.filter(value, messages)(_.id)       // None (LIKE on a LONG not supported)
  val query3 = LIKE.filter("Dave%", messages)(_.sender) // ok
  val query4 = IN.filter(List(1L, 3L), messages)(_.id)  // ok 
  val query5 = LIKE.filter("Affirmative%", messages)(_.content) // ok

  query4 match {
    case Some(q) =>
      println(s"Query: ${q.result.statements.mkString}")
      println(exec(q.result))
    case None   => 
      println("Not a valid thing")
  }

}
