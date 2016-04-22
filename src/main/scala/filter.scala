import slick.driver.H2Driver.api._
import scala.language.higherKinds

object Filters {

  //
  // The types of comparison we support:
  //
  sealed trait Comparison {
    // A comparison can filter if there is a "where builder" for the types involved
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
  // For a given type, T, how we apply a comparison for that type
  //
  // Not all types of comparison are supported, so the builder returns
  // an Option. Could be an Either/Xor, but we're just using Option
  // in this example, where None indicates we don't allow that kind of 
  // comparison.
  //
  sealed trait WhereBuilder[R,T] {
    def apply[M,U,C[_]](column: M => Rep[R], cmp: Comparison, v: T, q: Query[M,U,C]): Option[Query[M,U,C]]
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

  // TODO: Instant, UUID, Boolean, or whatever types we are interested in
}
