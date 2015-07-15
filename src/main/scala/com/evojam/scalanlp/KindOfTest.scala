package com.evojam.scalanlp

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scala.collection.JavaConversions._
import scala.io.StdIn

import com.evojam.scalanlp.model.{Venue, Artist, Query}
import epic.corpora.CONLLSequenceReader
import epic.sequences.{Segmentation, SemiCRF}
import epic.trees.Span
import nak.data.Example
import org.joda.time.DateTime
import org.ocpsoft.prettytime.nlp.PrettyTimeParser

trait DefaultNer {
  val ner: SemiCRF[Any, String] =
    epic.models.NerSelector.loadNer("en").get
}

object KindOfTest extends App with DefaultNer {
  def prepareString(in: String): IndexedSeq[String] =
    epic.preprocess.tokenize(in.toLowerCase)

  def subsequence[T](seq: IndexedSeq[T], beg: Int, end: Int): IndexedSeq[T] =
    for (i <- beg until end)
      yield seq(i)

  def unpackSegmentation[L, W](segmentation: Segmentation[L, W]): IndexedSeq[(L, IndexedSeq[W])] =
    segmentation.segments.map {
      case (label, span) => (label, subsequence(segmentation.words, span.begin, span.end))
    }

  def flatten[L, W](unpacked: IndexedSeq[(L, IndexedSeq[W])]): List[(L, String)] =
    unpacked.map {
      case (label, words) => (label, words.mkString(" "))
    }.toList

  def parseDate(dateStr: String): Option[DateTime] =
    new PrettyTimeParser().parse(dateStr)
      .toList
      .headOption
      .map(date => new DateTime(date))

  def getQuery(unpacked: List[(String, String)]): Query = {
    val artists = unpacked
      .filter(_._1 == "ARTIST")
      .map(tpl => Artist(tpl._2))
    val venues = unpacked
      .filter(_._1 == "VENUE")
      .map(tpl => Venue(tpl._2))
    val dates = unpacked
      .filter(_._1 == "DATE")
      .map(tpl => parseDate(tpl._2))
      .flatten

    Query(artists, venues, dates.headOption, dates.tail.headOption)
  }

  def train() {
    val standardTrain = CONLLSequenceReader.readTrain(new FileInputStream("src/main/resources/data.train")).toIndexedSeq

    def makeSegmentation(ex: Example[IndexedSeq[String], IndexedSeq[IndexedSeq[String]]]): Segmentation[Any, String] = {
      val segments = ex.label.foldLeft(List.empty[(String, Int, Int)]) {
        case (acc, label) => acc match {
          case head :: tail => head match {
            case (`label`, beg, end) => (label, beg, end + 1) :: tail
            case (nextLabel, beg, end) => (label, end, end + 1) :: head :: tail
          }
          case Nil => List((label, 0, 1))
        }
      }.reverse.map{
        case (label, beg, end) => (label, Span(beg, end))
      }.toIndexedSeq

      Segmentation(segments, ex.features.map(_.mkString), ex.id)
    }

    val t = standardTrain.map(makeSegmentation)

    val crf = SemiCRF.buildSimple(t).asInstanceOf[SemiCRF[String, String]]

    val oos = new ObjectOutputStream(
      new BufferedOutputStream(
        new GZIPOutputStream(
          new FileOutputStream("out.gz"))))

    try {
      oos.writeObject(crf)
    } finally {
      oos.close()
    }

    waitForInput(crf)
  }

  def waitForInput(crf: SemiCRF[String, String]) {
    while(true) {
      println("\nReady:")
      val str = StdIn.readLine()

      val tokenized: IndexedSeq[String] = epic.preprocess.tokenize(str.toLowerCase)
      val bestSequence: Segmentation[String, String] = crf.bestSequence(tokenized).asInstanceOf[Segmentation[String, String]]
      val unpacked: IndexedSeq[(String, IndexedSeq[String])] = unpackSegmentation(bestSequence)
      val flattened: List[(String, String)] = flatten(unpacked)
      val query: Query = getQuery(flattened)

      println(s"QUERY: $query")

      val out = crf.bestSequence(prepareString(str)).render
      println(s"\t$out")
    }
  }

  def load(file: String): SemiCRF[String, String] = {
    val gzipin = breeze.util.nonstupidObjectInputStream(
      new BufferedInputStream(
        new GZIPInputStream(
          new FileInputStream(file))))
    try {
      gzipin.readObject().asInstanceOf[SemiCRF[String, String]]
    } finally {
      gzipin.close()
    }
  }

//  train()
  waitForInput(load("out.gz"))
}
