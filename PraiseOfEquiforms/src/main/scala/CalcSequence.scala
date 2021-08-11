import de.sciss.tsp.LinKernighan

import java.io.{DataInputStream, File, FileInputStream}

object CalcSequence:
  def main(args: Array[String]): Unit =
    val fIn = new File("similarities.bin")
    val in  = new DataInputStream(new FileInputStream(fIn))
    val numPar = 14
    val m = try
      val sz  = in.readInt()
      assert (sz == (numPar * (numPar-1))/2, s"Unexpected matrix size $sz")
      Array.tabulate(numPar) { i =>
        Array.fill(numPar - (i + 1)) {
          in.readDouble()
        }
      }
    finally
      in.close()

    val maxSim = m.flatten.max
    println(s"Maximum similarity is $maxSim")

    val cost = Array.ofDim[Double](numPar, numPar)
    for
      vi <- 0        until numPar
      vj <- (vi + 1) until numPar
    do
      val vk  = vj - (vi + 1)
      val c   = maxSim - m(vi)(vk)
      cost(vi)(vj) = c
      cost(vj)(vi) = c

    val rnd     = new util.Random(0)
    val N       = 1000
    val tours   = Seq.tabulate(N) { x =>
      val tour0   = rnd.shuffle((0 until numPar).toVector).toArray
      val lk      = LinKernighan(cost, tour0)
      if x == 0 then println(s"Original cost: ${lk.tourCost}")
      lk.run()
      (lk.tourCost, lk.tour.toList)
    }
    val (bestCost, bestTour) = tours.minBy(_._1)
    println(s"Optimized cost: $bestCost")
    println(bestTour.map(_ + 1).mkString(","))  // 1-based index
