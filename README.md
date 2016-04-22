# Example of dynamic filtering with Slick where the type of comparison is itself dynamic.

The idea is that a comparison can be applied to a query, column, and value providing there is a way to do the comparison for the type of the value.

That is, we've implemented a set of comparisons (such as EQ, LIKE, IN, GT) with a filter method that takes a query, column, and value.
The filter method will compile if there is an instance of the Where Builder for the type of the value.

This means implementing each type of comparison for each kind of type we are interested in.

Not all comparisons are valid.  For example, you cannot LIKE on a Long.
For this reason, the filter method returns an Option[Query], where None indicates the comparison is not supported.

Example usage:

```
import Filters._

val cmp: Comparison = EQ // perhaps provided by user
val maybeQuery = cmp.filter(table, 1L)(_.id)

maybeQuery match {
  case Some(q) => ... run that q.result
  case None    => ... report that error
}
```

Run the example code with:

```
sbt run
```

