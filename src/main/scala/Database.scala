case class Database(tables: List[Table]) {
  override def toString: String = {
    tables.map(_.toString).mkString("\n")
  }

  def create(tableName: String): Database = {
    if (tables.exists(_.name == tableName))
      this
    else
      Database(tables :+ Table(tableName, List()))
  }

  def drop(tableName: String): Database = {
    if (!tables.exists(_.name == tableName))
      this
    else
      Database(tables.filter(_.name != tableName))
  }

  def selectTables(tableNames: List[String]): Option[Database] = {
    val temp = tableNames.flatMap(name => tables.filter(_.name == name))
    if (temp.size != tableNames.size) None
    else Option(Database(temp))
  }

  def join(table1: String, c1: String, table2: String, c2: String): Option[Table] = {
    val selectedTables = selectTables(List(table1, table2))

    selectedTables match {
      case Some(Database(List(t1, t2))) =>
        if (t1.data.isEmpty) return Some(t2)
        if (t2.data.isEmpty) return Some(t1)

        val bothTables = t1.data.flatMap { row1 =>
          t2.data.find(row2 => row1.get(c1) == row2.get(c2)) match {
            case Some(row2) => Some(row1 ++ row2.filter(_._1 != c2).map { case (k, v) =>
              k -> (if (row1.get(k).contains("") || row1.get(k) == row2.get(k)) v
              else if (row1.getOrElse(k, "") != "" && v != "") row1.getOrElse(k, "") + ";" + v
              else row1.getOrElse(k, "") + v)
            })
            case None => None
          }
        }

        val onlyFirstTable = t1.data
          .filterNot(row1 => t2.data.exists(row2 => row1.get(c1) == row2.get(c2)))
          .map(row1 => t2.header.filter(_ != c2).map(_ -> "").toMap ++ row1)

        val onlySecondTable = t2.data
          .filterNot(row2 => t1.data.exists(row1 => row1.get(c1) == row2.get(c2)))
          .map(row2 => t1.header.map(_ -> "").toMap ++
            Map(c1 -> row2.getOrElse(c2, "")) ++ row2.filter(_._1 != c2))

        val joinedData = bothTables ++ onlyFirstTable ++ onlySecondTable

        Some(Table(table1 + " " + table2, joinedData))
      case _ => None
    }
  }

  // Implement indexing here
  def apply(index: Int): Table = {
    tables(index)
  }
}
