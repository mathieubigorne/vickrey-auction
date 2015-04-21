package com.auction

import VickreyAuction.{AuctionResult, Buyer}
import org.scalatest._

class VickreyAuctionTest extends FlatSpec with Matchers {
  val reservePrice = 100

  val winner = Buyer("E", List(10, 140, 132, 135))
  val loser = Buyer("A",  List(110, 130))
  val secondLoser = Buyer("D", List(105, 115, 90))
  val poorBuyer = Buyer("Poor", List(50, 60))
  val veryPoorBuyer = Buyer("VeryPoor", List(10))

  it should "find the winner when several buyers reach the reserve price" in {
    VickreyAuction.closeBids(reservePrice, List(winner, loser, secondLoser)) should be(
      Some(AuctionResult("E", 130))
    )
  }

  it should "find the winner when only one buyer reaches the reserve price" in {
    // ASSUMPTION: if only one buyer takes bids above the reserve price, the
    // winning price is the lowest price above the reserve price

    VickreyAuction.closeBids(reservePrice, List(winner, poorBuyer)) should be(
      Some(AuctionResult("E", 132))
    )
  }

  it should "return nothing when no one reaches the reserve price" in {
    VickreyAuction.closeBids(reservePrice, List(poorBuyer, veryPoorBuyer)) should be(None)
  }

  it should "return nothing when no one takes bids" in {
    VickreyAuction.closeBids(reservePrice, List()) should be(None)
  }
}
