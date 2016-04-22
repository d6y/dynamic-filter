import slick.ast.BaseTypedType
import slick.driver.H2Driver.api._
import scala.language.higherKinds

object Filters {

  //
  // The types of comparison we support:
  //
  sealed trait Comparison {
    // A comparison can filter if there is a "where builder" for the types involved
    def filter[M, U, C[_], T, R](v: T, q: Query[M, U, C])(column: M => Rep[R])
      (implicit where: WhereBuilder[R, T]): Option[Query[M, U, C]] = 
        where(column, v, q).lift(this)
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
  trait WhereBuilder[R, -T] {
    def apply[M, U, C[_]](column: M => Rep[R], v: T, q: Query[M, U, C]): PartialFunction[Comparison, Query[M, U, C]]
  }

  class SameTypeWhereBuilder[T: BaseTypedType] extends WhereBuilder[T, T] {
    def otherOps[M, U, C[_]]
      (column: M => Rep[T], v: T, q: Query[M, U, C]): PartialFunction[Comparison, Query[M, U, C]] =
        PartialFunction.empty

    private[this] def sameTypeOps[M, U, C[_]]
      (column: M => Rep[T], v: T, q: Query[M, U, C]): PartialFunction[Comparison, Query[M, U, C]] = {
        case EQ   => q.filter(t => column(t) === v): Query[M, U, C]
        case NEQ  => q.filter(t => column(t) =!= v): Query[M, U, C]
        case LTEQ => q.filter(t => column(t)  <= v): Query[M, U, C]
        case LT   => q.filter(t => column(t)  <  v): Query[M, U, C]
        case GTEQ => q.filter(t => column(t)  >= v): Query[M, U, C]
        case GT   => q.filter(t => column(t)  >  v): Query[M, U, C]
      }

    final def apply[M, U, C[_]]
      (column: M => Rep[T], v: T, q: Query[M, U, C]): PartialFunction[Comparison, Query[M, U, C]] =
        sameTypeOps(column, v, q).orElse(otherOps(column, v, q))
  }

  class OptionWhereBuilder[T: BaseTypedType] extends WhereBuilder[Option[T], T] {
    def otherOps[M, U, C[_]]
      (column: M => Rep[Option[T]], v: T, q: Query[M, U, C]): PartialFunction[Comparison, Query[M, U, C]] =
        PartialFunction.empty

    private[this] def optionOps[M, U, C[_]]
      (column: M => Rep[Option[T]], v: T, q: Query[M, U, C]): PartialFunction[Comparison, Query[M, U, C]] = {
        case EQ   => q.filter(t => column(t) === v): Query[M, U, C]
        case NEQ  => q.filter(t => column(t) =!= v): Query[M, U, C]
        case LTEQ => q.filter(t => column(t)  <= v): Query[M, U, C]
        case LT   => q.filter(t => column(t)  <  v): Query[M, U, C]
        case GTEQ => q.filter(t => column(t)  >= v): Query[M, U, C]
        case GT   => q.filter(t => column(t)  >  v): Query[M, U, C]
      }

    final def apply[M, U, C[_]]
      (column: M => Rep[Option[T]], v: T, q: Query[M, U, C]): PartialFunction[Comparison, Query[M, U, C]] =
        optionOps(column, v, q).orElse(otherOps(column, v, q))
  }

  class SeqWhereBuilder[T: BaseTypedType] extends WhereBuilder[T, Seq[T]] {
    def otherOps[M, U, C[_]]
      (column: M => Rep[T], v: Seq[T], q: Query[M, U, C]): PartialFunction[Comparison, Query[M, U, C]] =
        PartialFunction.empty

    private[this] def seqOps[M, U, C[_]]
      (column: M => Rep[T], v: Seq[T], q: Query[M, U, C]): PartialFunction[Comparison, Query[M, U, C]] = {
        case IN => q.filter(t => column(t) inSetBind v)
      }

    final def apply[M, U, C[_]]
      (column: M => Rep[T], v: Seq[T], q: Query[M, U, C]): PartialFunction[Comparison, Query[M, U, C]] =
        seqOps(column, v, q).orElse(otherOps(column, v, q))
  }

  object WhereBuilder extends LowPriorityWhereBuilderInstances {
    implicit val stringWhereBuilder: WhereBuilder[String, String] = new SameTypeWhereBuilder[String] {
      override def otherOps[M, U, C[_]]
        (column: M => Rep[String], v: String, q: Query[M, U, C]): PartialFunction[Comparison, Query[M, U, C]] = {
          case LIKE => q.filter(t => column(t) like v)
        }
    }

    implicit val stringOptionWhereBuilder: WhereBuilder[Option[String], String] =
      new OptionWhereBuilder[String] {
        override def otherOps[M, U, C[_]]
          (column: M => Rep[Option[String]], v: String, q: Query[M, U, C]): PartialFunction[Comparison, Query[M, U, C]] = {
            case LIKE => q.filter(t => column(t) like v)
          }
    }
  }

  class LowPriorityWhereBuilderInstances {
    implicit def anyWhereBuilder[T: BaseTypedType]: WhereBuilder[T, T] = new SameTypeWhereBuilder[T]
    implicit def anyOptionWhereBuilder[T: BaseTypedType]: WhereBuilder[Option[T], T] = new OptionWhereBuilder[T]
    implicit def anySeqWhereBuilder[T: BaseTypedType]: WhereBuilder[T, Seq[T]] = new SeqWhereBuilder[T]
  }

  // TODO: Instant, UUID, Boolean, or whatever types we are interested in
}
