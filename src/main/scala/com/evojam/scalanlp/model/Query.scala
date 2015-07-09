package com.evojam.scalanlp.model

import org.joda.time.DateTime

case class Query(
  artists: List[Artist],
  venues: List[Venue],
  dateFrom: Option[DateTime],
  dateTo: Option[DateTime]) {

  require(artists != null, "artists cannot be null")
  require(venues != null, "venues cannot be null")
  require(dateFrom != null, "dateFrom cannot be null")
  require(dateTo != null, "dateTo cannot be null")
}

case class Artist(value: String)

case class Venue(value: String)