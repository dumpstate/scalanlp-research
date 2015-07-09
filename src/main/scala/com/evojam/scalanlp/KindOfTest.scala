package com.evojam.scalanlp

import java.io.FileInputStream

import scala.collection.JavaConversions._
import scala.io.StdIn

import com.evojam.scalanlp.model.{Venue, Artist, Query, Label}
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

  def getQuery(unpacked: List[(Label, String)]): Query = {
    val artists = unpacked
      .filter(_._1 == Label("ARTIST"))
      .map(tpl => Artist(tpl._2))
    val venues = unpacked
      .filter(_._1 == Label("VENUE"))
      .map(tpl => Venue(tpl._2))
    val dates = unpacked
      .filter(_._1 == Label("DATE"))
      .map(tpl => parseDate(tpl._2))
      .flatten

    Query(artists, venues, dates.headOption, dates.tail.headOption)
  }

  def train() {
    val standardTrain = CONLLSequenceReader.readTrain(new FileInputStream("src/main/resources/data.train")).toIndexedSeq

    def makeSegmentation(ex: Example[IndexedSeq[String], IndexedSeq[IndexedSeq[String]]]): Segmentation[Any, String] = {
      val segments = ex.label.foldLeft(List.empty[(Label, Int, Int)]) {
        case (acc, label) => acc match {
          case head :: tail => head match {
            case (Label(`label`), beg, end) => (Label(label), beg, end + 1) :: tail
            case (Label(nextLabel), beg, end) => (Label(label), end, end + 1) :: head :: tail
          }
          case Nil => List((Label(label), 0, 1))
        }
      }.reverse.map{
        case (label, beg, end) => (label, Span(beg, end))
      }.toIndexedSeq

      Segmentation(segments, ex.features.map(_.mkString), ex.id)
    }

    val t = standardTrain.map(makeSegmentation)

    val crf = SemiCRF.buildSimple(t)

    while(true) {
      println("\nReady:")
      val str = StdIn.readLine()

      val tokenized: IndexedSeq[String] = epic.preprocess.tokenize(str.toLowerCase)
      val bestSequence: Segmentation[Label, String] = crf.bestSequence(tokenized).asInstanceOf[Segmentation[Label, String]]
      val unpacked: IndexedSeq[(Label, IndexedSeq[String])] = unpackSegmentation(bestSequence)
      val flattened: List[(Label, String)] = flatten(unpacked)
      val query: Query = getQuery(flattened)

      println(s"QUERY: $query")

      val out = crf.bestSequence(prepareString(str)).render
      println(s"\t$out")
    }
  }

  train()
}
