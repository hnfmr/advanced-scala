package io.hnfmr.chapter11

final case class GCounterNaive(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int): GCounterNaive =
    GCounterNaive(counters + (machine -> (amount + counters.getOrElse(machine, 0))))

  def get: Int = counters.values.sum

  def merge(that: GCounterNaive): GCounterNaive = {
    val mergedCounters =
      that.counters ++ {
        for ( (k,v) <- counters) yield {
          k -> (v max that.counters.getOrElse(k, 0))
        }
      }

    GCounterNaive(mergedCounters)
  }
}