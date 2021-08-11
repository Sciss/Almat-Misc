import java.io.{DataOutputStream, File, FileInputStream, FileOutputStream}
import scala.sys.process.*

object CalcSimilarities:
  def main(args: Array[String]): Unit =
    val fIn   = new File("auto-trans-en.txt")
    val fOut  = new File("similarities.bin")

    if fOut.exists() then
      println("Output file already exists. Not overwriting!")
      sys.exit()

    val src = io.Source.fromFile(fIn, "UTF-8")
    val par = try
      src.getLines().map(_.trim).filter(_.nonEmpty).toList
    finally
      src.close()

    assert (par.size == 14, "Expected 14 paragraphs")

    def escape(s: String) = s.replace("'", "\\'")

    val simList = for
      i <- 0     until par.size
      j <- i + 1 until par.size
    yield
      val p1 = escape(par(i))
      val p2 = escape(par(j))
      // yes, it's ultra-slow to re-read the model for every pair,
      // but I'm too lazy to learn how to do I/O in Python in this life, not worth it.
      val pythonCode =
        s"""from semantic_text_similarity.models import WebBertSimilarity
           |web_model = WebBertSimilarity(device='cpu', batch_size=10)
           |x = web_model.predict([('$p1', '$p2')])
           |print(x[0])
           |""".stripMargin

      val cmd = Seq("python3", "-c", pythonCode)
      val out = cmd.!!
      val sim = out.trim.toDouble
      println(s"Pair ($i, $j) yields $sim")
      sim

    val os = new DataOutputStream(new FileOutputStream(fOut))
    try
      os.writeInt(simList.size)
      simList.foreach(os.writeDouble)
    finally
      os.close()

  end main
