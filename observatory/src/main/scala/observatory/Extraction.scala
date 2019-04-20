package observatory

import java.time.LocalDate

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    Main.initialize()
    Main.locateTemperatures(year, stationsFile, temperaturesFile).collect().toIterable
    .map(row => (LocalDate.of(year, row.getInt(0), row.getInt(1)), Location(row.getDouble(2), row.getDouble(3)), Main.toCelsius(row.getDouble(4))))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.groupBy(x => x._2).mapValues(it => it.map(x => x._3).sum / it.size)
  }

}
