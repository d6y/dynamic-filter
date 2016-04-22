import slick.driver.H2Driver.api._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Example extends App {

  final case class Message(
    sender:  String,
    content: String,
    id:      Long = 0L)

  def freshTestData = Seq(
    Message("Dave", "Hello, HAL. Do you read me, HAL?"),
    Message("HAL",  "Affirmative, Dave. I read you."),
    Message("Dave", "Open the pod bay doors, HAL."),
    Message("HAL",  "I'm sorry, Dave. I'm afraid I can't do that.")
  )

  final class MessageTable(tag: Tag)
      extends Table[Message](tag, "message") {

    def id      = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def sender  = column[String]("sender")
    def content = column[String]("content")

    def * = (sender, content, id) <> (Message.tupled, Message.unapply)
  }

  lazy val messages = TableQuery[MessageTable]

  val db = Database.forConfig("example")

  def exec[T](program: DBIO[T]): T = Await.result(db.run(program), 2 seconds)

  exec(messages.schema.create)
  exec(messages ++= freshTestData)

  import scala.language.higherKinds

  //
  // The types of comparison we support:
  //
  sealed trait Comparison {
    def filter[M,U,C[_],T,R](v: T, q: Query[M,U,C])(column: M => Rep[R])(implicit where: WhereBuilder[R,T]): Option[Query[M,U,C]] = 
      where(column, this, v, q)
  }
  case object EQ   extends Comparison
  case object NEQ  extends Comparison
  case object LT   extends Comparison
  case object LTEQ extends Comparison
  case object GT   extends Comparison
  case object GTEQ extends Comparison
  case object LIKE extends Comparison
  case object IN   extends Comparison


  //
  // For a given type, T, how we apply a comparison for that type:
  //
  sealed trait WhereBuilder[T,R] {
    def apply[M,U,C[_]](column: M => Rep[T], cmp: Comparison, v: R, q: Query[M,U,C]): Option[Query[M,U,C]]
  }

  implicit object WhereLong extends WhereBuilder[Long,Long] {
    def apply[M,U,C[_]](column: M => Rep[Long], cmp: Comparison, v: Long, q: Query[M,U,C]): Option[Query[M,U,C]] =
      cmp match {
        case EQ   => Some(q.filter(t => column(t) === v))
        case NEQ  => Some(q.filter(t => column(t) =!= v))
        case LTEQ => Some(q.filter(t => column(t)  <= v))
        case LT   => Some(q.filter(t => column(t)  <  v))
        case GTEQ => Some(q.filter(t => column(t)  >= v))
        case GT   => Some(q.filter(t => column(t)  >  v))
        case IN   => None
        case LIKE => None
      }
  }

  implicit object WhereLongSeq extends WhereBuilder[Long, List[Long]] {
    def apply[M,U,C[_]](column: M => Rep[Long], cmp: Comparison, vs: List[Long], q: Query[M,U,C]): Option[Query[M,U,C]] =
      cmp match {
        case IN => Some(q.filter(t => column(t) inSetBind vs))
        case _  => None
      }
  }
    
  implicit object WhereString extends WhereBuilder[String, String] {
    def apply[M,U,C[_]](column: M => Rep[String], cmp: Comparison, v: String, q: Query[M,U,C]): Option[Query[M,U,C]] =
      cmp match {
        case EQ   => Some(q.filter(t => column(t) === v))
        case NEQ  => Some(q.filter(t => column(t) =!= v))
        case LTEQ => Some(q.filter(t => column(t)  <= v))
        case LT   => Some(q.filter(t => column(t)  <  v))
        case GTEQ => Some(q.filter(t => column(t)  >= v))
        case GT   => Some(q.filter(t => column(t)  >  v))
        case LIKE => Some(q.filter(t => column(t) like v))
        case IN   => None
    }
  }

  // TODO: Instant, UUID, Boolean
  //

  //
  //Examples usage:
  //

  val cmp: Comparison = EQ
  val value = 1L

  val query1 = cmp.filter(value, messages)(_.id)        // ok
  val query2 = LIKE.filter(value, messages)(_.id)       // not valid (LIKE on a LONG)
  val query3 = LIKE.filter("Dave%", messages)(_.sender) // ok
  val query4 = IN.filter(List(1L, 3L), messages)(_.id)  // ok 

  query4 match {
    case Some(q) =>
      println(s"Query: ${q.result.statements.mkString}")
      println(exec(q.result))
    case None   => 
      println("Not a valid thing")
  }

}
