object Queries {

  def killJackSparrow(t: Table): Option[Table] = queryT(Some(t), "FILTER", Not(Field("name", _ == "Jack")))

  def insertLinesThenSort(db: Database): Option[Table] = {
    queryDB((Some(db), "CREATE", "Inserted Fellas"))
      .flatMap(db => queryDB((Some(db), "SELECT", List("Inserted Fellas"))))
      .flatMap(db_1 => List(
        Map("name" -> "Ana", "age" -> "93", "CNP" -> "455550555"),
        Map("name" -> "Diana", "age" -> "33", "CNP" -> "255532142"),
        Map("name" -> "Tatiana", "age" -> "55", "CNP" -> "655532132"),
        Map("name" -> "Rosmaria", "age" -> "12", "CNP" -> "855532172")
      ).foldLeft(Some(db_1.tables.head): Option[Table])((table, row) => queryT((table, "INSERT", List(row)))))
      .flatMap(table => queryT((Some(table), "SORT", "age")))
  }

  def youngAdultHobbiesJ(db: Database): Option[Table] = {
    queryDB((Some(db), "JOIN", "People", "name", "Hobbies", "name"))
    .flatMap(db => queryT((Some(db.tables.head), "FILTER", All(List(Field("age", age =>
        age != "" && age.toInt < 25), Field("name", _.startsWith("J")), Field("hobby", _ != "")))))
    .flatMap(table => queryT((Some(table), "EXTRACT", List("name", "hobby")))))
  }
}
