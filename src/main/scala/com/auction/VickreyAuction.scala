package com.auction

object VickreyAuction {

  case class Buyer(name: String, bids: List[Int])
  case class AuctionResult(winner: String, winningPrice: Int)

  def Descending[T : Ordering] = implicitly[Ordering[T]].reverse

  def closeBids(reservePrice: Int, buyers: List[Buyer]): Option[AuctionResult] =
    getBuyersWithValidBids(buyers, reservePrice)
      .sortBy(_.bids.max)(Descending) match {
        case highestBuyer :: secondBuyer :: _ =>
          Some(AuctionResult(highestBuyer.name, secondBuyer.bids.max))
        case highestBuyer :: _ =>
          Some(AuctionResult(highestBuyer.name, highestBuyer.bids.min))
        case _ => None
      }

  // A bid is valid if it is above the reserve price
  private def getBuyersWithValidBids(buyers: List[Buyer], reservePrice: Int) =
    buyers flatMap (buyer => {
      val bidsAboveReservePrice = buyer.bids.filter(_ > reservePrice)
      if (bidsAboveReservePrice.nonEmpty) {
        Some(buyer.copy(bids = bidsAboveReservePrice))
      } else {
        None
      }
    })
}
