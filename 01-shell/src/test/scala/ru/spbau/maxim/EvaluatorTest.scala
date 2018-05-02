package ru.spbau.maxim

import org.scalatest.{FunSuite, Matchers}

import scala.reflect.io.Path

class EvaluatorTest extends FunSuite with Matchers {
  test("echoWcTest") {
    val evaluator = new Evaluator
    evaluator.evaluatePipeline("echo '1  23 45' | wc") should be ("1 3 8")
  }

  test("exitTest") {
    val evaluator = new Evaluator
    evaluator.evaluatePipeline("exit | echo 1 | wc") should be ("Exit")
    evaluator.evaluatePipeline("echo '1  23 45' | wc") should be ("")
  }

  test("catWcTest") {
    val fileText =
      """|Корабли лавировали, лавировали, лавировали
         |да не вылавировали""".stripMargin

    val tmpDir = Path(System.getProperty("java.io.tmpdir"))
    val fileName = "cat_tmp.txt"
    val tmpFile = tmpDir.resolve(fileName)
    println(tmpFile.toAbsolute)
    if (!tmpFile.exists) {
      tmpFile.createFile(failIfExists = true)
      tmpFile.toFile.printlnAll(fileText)
    }
    val evaluator = new Evaluator
    evaluator.evaluatePipeline(s"cat ${tmpFile.path}") should be (fileText)

    evaluator.evaluatePipeline(s"cat ${tmpFile.path} ${tmpFile.path}") should be (fileText + "\n" + fileText)

    evaluator.evaluatePipeline(s"wc ${tmpFile.path}") should be ("2 7 61")
  }

  test("lsTest") {
    val tmpDir = Path(System.getProperty("java.io.tmpdir"))
    val fileName = "file.txt"
    val tmpFile = tmpDir.resolve(fileName)
    tmpFile.createFile()

    val evaluator = new Evaluator
    val files = evaluator.evaluatePipeline(s"ls ${tmpDir.toAbsolute.path}").split("\n")
    files should contain (fileName)
  }

  test("cdTest") {
    val tmpDir = Path(System.getProperty("java.io.tmpdir"))
    val evaluator = new Evaluator
    evaluator.evaluatePipeline(s"cd ${tmpDir.toAbsolute.path}") should be ("")
    evaluator.evaluatePipeline(s"pwd") should be (tmpDir.toAbsolute.path)
  }

  test("assignTest") {
    val evaluator = new Evaluator

    evaluator.evaluatePipeline("a='1 '")
    Model.getEnvValue("a") should be ("1 ")

    evaluator.evaluatePipeline("a = exit ")
    Model.getEnvValue("a") should be ("exit")

    evaluator.evaluatePipeline("echo $a") should be ("exit")
  }
}
