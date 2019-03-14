package bootstrap

import collection.mutable.ArrayBuffer

/**
  * Quick tool to compute statistical significance using bootstrap resampling
  * User: peter
  * Date: 9/18/13
  */
object BootstrapResampling {

  def computeBootstrapResampling(baseline:Array[Double], experimental:Array[Double], numSamples:Int = 10000): Double = {
    val rand = new java.util.Random()

    // Step 1: Check input sizes of baseline and experimental arrays are the same
    if (baseline.size != experimental.size) throw new RuntimeException("BootstrapResampling.computeBootstrapResampling(): ERROR: scoresBefore and scoresAfter have different lengths")
    val numDataPoints = baseline.size

    // Step 2: compute difference scores
    val deltas = new ArrayBuffer[Double]
    for (i <- 0 until baseline.size) {
      val delta = experimental(i) - baseline(i)
      deltas.append(delta)
    }

    // Step 3: Resample 'numSample' times, computing the mean each time.  Store the results.
    val means = new ArrayBuffer[Double]
    for (i <- 0 until numSamples) {
      var mean: Double = 0.0
      for (j <- 0 until numDataPoints) {
        val randIdx = rand.nextInt(numDataPoints)
        mean = mean + deltas(randIdx)
      }
      mean = mean / numDataPoints
      means.append(mean)
    }

    // Step 4: Compute proportion of means at or below 0 (the null hypothesis)
    var proportionBelowZero: Double = 0.0
    for (i <- 0 until numSamples) {
      println ("bootstrap: mean: " + means(i))
      if (means(i) <= 0) proportionBelowZero += 1
    }
    proportionBelowZero = proportionBelowZero / numSamples

    // debug
    println("Proportion below zero: " + proportionBelowZero)

    // Return the p value
    proportionBelowZero
  }

  // Create artificial baseline and experimental arrays that contain a certain number of samples (numSamples),
  // with some number that are helped by the experimental model (numHelped), and some that are hurt (numHurt).
  def makeArtificialData(numSamples:Int, numHelped:Int, numHurt:Int):(Array[Double], Array[Double]) = {
    val baseline = Array.fill[Double](numSamples)(0.0)
    val experimental = Array.fill[Double](numSamples)(0.0)

    // Add helped
    for (i <- 0 until numHelped) {
      experimental(i) = 1     // Answered correct by the experimental model (but not the baseline model)
    }

    // Add hurt
    for (i <- numHelped until (numHelped + numHurt)) {
      baseline(i) = 1         // Answered correctly by the baseline model (but not the experimental model)
    }

    // Return
    (baseline, experimental)
  }


  def main(args:Array[String]): Unit = {
    // Example 1: Manually entering data
    val baseline = Array[Double](0, 1, 1, 0, 0, 1, 0, 1, 0, 1)
    val experimental = Array[Double](1, 1, 0, 1, 1, 0, 1, 1, 0, 0)
    val p = computeBootstrapResampling(baseline, experimental)
    println ("p-value: " + p)

    // Example 2: Generating artificial data
    val (baseline1, experimental1) = makeArtificialData(numSamples = 500, numHelped = 10, numHurt = 7)
    val p1 = computeBootstrapResampling(baseline1, experimental1)
    println ("p-value: " + p1)
  }

}

