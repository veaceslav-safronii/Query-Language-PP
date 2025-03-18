import scala.language.implicitConversions

trait FilterCond {def eval(r: Row): Option[Boolean]}

case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =  r.get(colName).map(predicate)
}

case class Compound(op: (Boolean, Boolean) => Boolean, conditions: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = {
    conditions.map(_.eval(r)).reduce((a, b) => {
      (a, b) match {
        case (Some(x), Some(y)) => Some(op(x, y))
        case _ => None
      }
    })
  }
}

case class Not(f: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean] =
    f.eval(r) match {
      case Some(x) => Some(!x)
      case _ => None
    }
}

def And(f1: FilterCond, f2: FilterCond): FilterCond = {
  Compound(_ && _, List(f1, f2))
}
def Or(f1: FilterCond, f2: FilterCond): FilterCond = {
  Compound(_ || _, List(f1, f2))
}
def Equal(f1: FilterCond, f2: FilterCond): FilterCond =
  Compound(_ == _, List(f1, f2))

case class Any(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = fs.foldLeft(Option(false))((acc, cond) => {
    (acc, cond.eval(r)) match {
      case (Some(a), Some(b)) => Some(a || b)
      case _ => None
    }
  })
}

case class All(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean] = fs.foldLeft(Option(true))((acc, cond) => {
    (acc, cond.eval(r)) match {
      case (Some(a), Some(b)) => Some(a && b)
      case _ => None
    }
  })
}

implicit def tuple2Field(t: (String, String => Boolean)): Field = {
  Field(t._1, t._2)
}

extension (f: FilterCond) {
  def ===(other: FilterCond) = Equal(f, other)
  def &&(other: FilterCond) = And(f, other)
  def ||(other: FilterCond) = Or(f, other)
  def !! = Not(f)
}