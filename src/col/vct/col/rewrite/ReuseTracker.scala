package vct.col.rewrite

import java.nio.file.{Files, Paths}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import scala.jdk.CollectionConverters._

/** Tracks how often node rewrites are skipped because the node can be reused
  * unchanged, versus how often a new node is allocated. Statistics are keyed by
  * (passName, nodeType) and are written to a file after all translation passes
  * have finished.
  */
object ReuseTracker {
  private val reusedCounts =
    new ConcurrentHashMap[(String, String), AtomicLong]()
  private val totalCounts =
    new ConcurrentHashMap[(String, String), AtomicLong]()
  // Maps passName -> insertion order so output follows pass order
  private val passOrder = new ConcurrentHashMap[String, Int]()
  private val passCounter = new AtomicLong(0)
  private val disabled = true

  /** The name of the rewrite pass that is currently executing. Set from
    * [[vct.main.stages.Transformation]] before each pass.
    */
  private val currentPass = new AtomicReference[String]("(unknown)")

  def setCurrentPass(name: String): Unit = {
    if (disabled)
      return
    currentPass.set(name)
    passOrder.computeIfAbsent(name, _ => passCounter.getAndIncrement().toInt)
  }

  def record(nodeType: String, wasReused: Boolean): Unit = {
    if (disabled)
      return
    val pass = currentPass.get()
    totalCounts.computeIfAbsent((pass, nodeType), _ => new AtomicLong(0))
      .incrementAndGet()
    if (wasReused)
      reusedCounts.computeIfAbsent((pass, nodeType), _ => new AtomicLong(0))
        .incrementAndGet()
  }

  private def pct(reused: Long, total: Long): String =
    if (total == 0)
      "N/A"
    else
      "%.1f%%".format(reused * 100.0 / total)

  /** Formats and writes the report to `reuse-stats.txt` in the working
    * directory, then resets all internal state. Returns the report string.
    */
  def reportAndReset(): String = {
    if (disabled)
      return ""
    // Snapshot both maps
    val allKeys =
      (reusedCounts.keySet().asScala ++ totalCounts.keySet().asScala).toSet
    case class Entry(pass: String, node: String, reused: Long, total: Long)
    val snapshot: Seq[Entry] = allKeys.toSeq.map { case key @ (pass, node) =>
      Entry(
        pass,
        node,
        Option(reusedCounts.get(key)).map(_.get()).getOrElse(0L),
        Option(totalCounts.get(key)).map(_.get()).getOrElse(0L),
      )
    }
    reusedCounts.clear()
    totalCounts.clear()

    val orderedPasses = passOrder.asScala.toSeq.sortBy(_._2).map(_._1)
    passOrder.clear()
    passCounter.set(0)
    currentPass.set("(unknown)")

    if (snapshot.isEmpty)
      return "(no reuse recorded)"

    val sb = new StringBuilder
    sb.append(
      "=== Node Reuse Statistics (rewriteDefault short-circuited) ===\n"
    )

    // Per-pass breakdown
    sb.append("\n--- Per Pass ---\n")
    for (pass <- orderedPasses) {
      val passData = snapshot.filter(_.pass == pass).sortBy(-_.reused)
      val passReused = passData.map(_.reused).sum
      val passTotal = passData.map(_.total).sum
      if (passTotal > 0) {
        sb.append("  %s  %s / %s (%s)\n".format(
          pass,
          "%,d".format(passReused),
          "%,d".format(passTotal),
          pct(passReused, passTotal),
        ))
        for (e <- passData)
          sb.append("    %-60s %s / %s (%s)\n".format(
            e.node,
            "%,d".format(e.reused),
            "%,d".format(e.total),
            pct(e.reused, e.total),
          ))
      }
    }

    // Per node type totals across all passes
    val perNode = snapshot.groupBy(_.node).view
      .mapValues(es => (es.map(_.reused).sum, es.map(_.total).sum)).toSeq
      .sortBy(-_._2._2)
    val grandReused = snapshot.map(_.reused).sum
    val grandTotal = snapshot.map(_.total).sum
    sb.append("\n--- Per Node Type (all passes combined) ---\n")
    for ((node, (reused, total)) <- perNode)
      sb.append("  %-60s %s / %s (%s)\n".format(
        node,
        "%,d".format(reused),
        "%,d".format(total),
        pct(reused, total),
      ))

    sb.append("\nGrand total: %s / %s (%s)\n".format(
      "%,d".format(grandReused),
      "%,d".format(grandTotal),
      pct(grandReused, grandTotal),
    ))

    val report = sb.toString()
    Files.writeString(Paths.get("reuse-stats.txt"), report)
    report
  }
}
