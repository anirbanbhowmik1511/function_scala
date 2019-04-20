package object forcomp {
  val dictionaryPath = List("forcomp", "linuxwords.txt")

  def loadDictionary = {
//    val wordstream = Option {
//      getClass.getResourceAsStream(dictionaryPath.mkString("/"))
//    } orElse {
//      common.resourceAsStreamFromSrc(dictionaryPath)
//    } getOrElse {
//      sys.error("Could not load word list, dictionary file not found")
//    }
    try {
      val s = io.Source.fromFile("/home/anirban/workspace/forcomp/src/main/resources/forcomp/linuxwords.txt")
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      //wordstream.close()
    }
  }

}
