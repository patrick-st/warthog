package org.warthog.pbl.benchmark

import java.io.FileWriter
import java.util.concurrent._

/**
 * Class executes a given benchmark
 * @param benchmark to execute
 */
class BenchmarkExecuteor(benchmark: Benchmark) {

  val ls = System.getProperty("line.separator")

  /**
   * Method to execute the given benchmark
   * After the given timeout the execution is aborted
   * @param executionTime maximal computation time
   * @param fw FileWriter to write the computed result to a file
   */
    def execute(executionTime: Int, fw: FileWriter): Unit = {
      val start = System.currentTimeMillis()
      val service: ExecutorService = Executors.newSingleThreadExecutor()
      val f = service.submit(benchmark)
      try {
        f.get(executionTime, TimeUnit.MINUTES)
        val end = System.currentTimeMillis()
        val duration = round((end - start) / 1000.0)
        val result = benchmark.path + "," + benchmark.softClauses + "," + benchmark.hardClauses + "," + benchmark.minUnsatOptimum + "," + duration + ls
        fw.write(result)
      }
      catch {
        case tOut: TimeoutException => {
          //computation took to much time
          val result = benchmark.path + "," + benchmark.softClauses + "," + benchmark.hardClauses + "," + "-" + "," + "> 600" + ls
          fw.write(result)
        }
      } finally {
        f.cancel(true)
        service.shutdown()
        service.shutdownNow()
      }
    }


  private def round(d: Double) = {
    Math.round(d * 100)/100.0
  }
}

object Main {

  def main (args: Array[String]) {
    val fs = System.getProperty("file.separator")
    val ls = System.getProperty("line.separator")
    val subdirectory = "pMaxSAT" + fs + "use_case_user_selections" + fs + "B_1" + fs + "30"
    val readSubdirectory = "src" + fs + "test" + fs + "resources" + fs + "pbl" + fs + "benchmark_instances" + fs + subdirectory
    //List of paths of test files
    var testFiles = List[String](
      "instance_0000.wcnf",
      "instance_0001.wcnf",
      "instance_0002.wcnf",
      "instance_0003.wcnf",
      "instance_0004.wcnf",
      "instance_0005.wcnf",
      "instance_0006.wcnf",
      "instance_0007.wcnf",
      "instance_0008.wcnf",
      "instance_0009.wcnf"
    )
    val optimiser = Optimiser.BinaryMaxSAT
    val learnMethod = null
    //destination to write result file
    val destination = "C:" + fs + "Users" + fs + "Patrick" + fs + "Desktop" + fs + "Masterarbeit" + fs + "Benchmark" + fs + "results" + fs + subdirectory + fs + optimiser +"_" + learnMethod + ".csv"
    val fw = new FileWriter(destination, true)
    fw.write("path,# soft clauses,# hard clauses,min unsat optimum,duration" + ls)
    val executionTime = 10
    var benchmark: Benchmark = null
    var executor: BenchmarkExecuteor = null
    while(testFiles.nonEmpty){
      benchmark = new Benchmark(readSubdirectory + fs + testFiles.head,optimiser,learnMethod)
      executor = new BenchmarkExecuteor(benchmark)
      executor.execute(executionTime,fw)
      testFiles = testFiles.tail
    }
    fw.close()
    System.exit(0)
  }
}
