type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {

  override def toString: String = header.mkString(",") + "\n" +
    data.map(_.values.toList.mkString(",")).mkString("\n")

  def insert(row: Row): Table = {
    if (!data.contains(row))
      Table(tableName, data :+ row)
    else this
  }

  def delete(row: Row): Table = Table(tableName, data.filter(_ != row))

  def sort(column: String): Table = Table(tableName, data.sortBy(_.get(column)))

  def update(f: FilterCond, updates: Map[String, String]): Table = {
    Table(tableName, data.map(row => if (f.eval(row).getOrElse(false)) row ++ updates else row))
  }

  def filter(f: FilterCond): Table = {
    Table(tableName, data.filter(f.eval(_).getOrElse(false)))
  }

  def select(columns: List[String]): Table =
    new Table(tableName, data.map(_.filter(m => columns.contains(m._1))))

  def header: List[String] = tableData.head.keys.toList
  def data: Tabular = tableData
  def name: String = tableName
}

object Table {
  def apply(name: String, s: String): Table = {
    val lines = s.split("\n").toList
    val head = lines.head.split(",").toList
    new Table(name, lines.tail.map(_.split(",").toList).map(head.zip(_).toMap))
  }
}

extension (table: Table) {
  def apply(i: Int): Table = // Implement indexing here, find the right function to override
    Table(table.name, List(table.data(i)))
}
