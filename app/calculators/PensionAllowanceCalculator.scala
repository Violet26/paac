/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package calculators

import models._
import models.PensionPeriod._
import play.api.Logger
import calculators.results.Calculator
import scala.util.{Try, Success, Failure}

trait PensionAllowanceCalculator {

  protected def getPeriodContributions(contributions : Seq[Contribution],
                                       missingRowsAreRegistered: Boolean = true,
                                       notMemberInP1: Boolean = false): List[Contribution] = {
    val inputsByTaxYear = contributions.groupBy(_.taxPeriodStart.year)

    def getPeriodContribution(isP1: Boolean): List[Contribution] = {
      val contributions = if (inputsByTaxYear.contains(YEAR_2015)) {
          inputsByTaxYear(YEAR_2015).groupBy(_.isPeriod1)
        } else {
          Map[Boolean,Seq[Contribution]]()
        }
      val amount = if (missingRowsAreRegistered)
                     0L
                   else
                     Contribution.periodAllowance(isP1)
      contributions.get(isP1).map{ cs =>
        cs.toList.map { c =>
          if (notMemberInP1 && isP1) {
            c.copy(amounts=c.amounts.map{
              case InputAmounts(Some(_), Some(_), i, t) => InputAmounts(Some(4000000L), Some(4000000L), i, t)
              case InputAmounts(Some(_), None, i, t) => InputAmounts(Some(8000000L), None, i, t)
              case InputAmounts(None, Some(_), i, t) => InputAmounts(None, Some(8000000L), i, t)
              case a @ _ => a
            })
          } else {
            c
          }
        }
      }.getOrElse(List(Contribution(isP1, amount, 0)))
    }

    def addContribution(start: PensionPeriod,
                        end: PensionPeriod,
                        isP1: Boolean,
                        lst1: List[Contribution],
                        lst2: List[Contribution]): List[Contribution] = {
      val newPreTrigger = Contribution(start, end, if (missingRowsAreRegistered) 0L else Contribution.periodAllowance(isP1), 0, false)
      List(newPreTrigger) ++ lst1 ++ lst2
    }

    val p1 = getPeriodContribution(true)
    val p2 = getPeriodContribution(false)

    // whenever there is a triggered contribution then we must have 3 contributions over p1 and p2,
    // one of which must be a pre-trigger contribution even if inputs are 0
    if (p1.size == 1 && p1(0).isTriggered) {
      addContribution(PERIOD_1_2015_START, p1(0).taxPeriodStart, true, p1, p2)
    } else if (p2.size == 1 && p2(0).isTriggered) {
      if (p1.size == 1 && !p1(0).isTriggered) {
        addContribution(PERIOD_2_2015_START, p2(0).taxPeriodStart, false, p2, p1)
      } else {
        p1 ++ p2
      }
    } else {
      p1 ++ p2
    }
  }

  protected def provideMissingYearContributions(contributions : Seq[Contribution],
                                                earliestYear: Int = EARLIEST_YEAR_SUPPORTED,
                                                missingRowsAreRegistered: Boolean = true,
                                                notMemberInP1: Boolean = false): List[Contribution] = {
    val inputsByTaxYear = contributions.groupBy(_.taxPeriodStart.year)
    val allContributions = ((earliestYear).min(inputsByTaxYear.keys.min) to inputsByTaxYear.keys.max).foldLeft(List[Contribution]()) {
      (lst:List[Contribution], year:Int) =>
        if (year != 2015) {
          val amount = if (missingRowsAreRegistered) 0L else Contribution.allowance(year)
          val contribution = inputsByTaxYear.get(year).getOrElse(List(Contribution(year,amount))).toList
          contribution ++ lst
        } else {
          getPeriodContributions(contributions,missingRowsAreRegistered,notMemberInP1) ++ lst
        }
    }
    allContributions.sortWith(Contribution.sortByYearAndPeriod _)
  }

  def calculateAllowances(contributions : Seq[Contribution],
                          doCollate: Boolean = false,
                          earliestYear: Int = EARLIEST_YEAR_SUPPORTED,
                          missingRowsAreRegistered: Boolean = true,
                          notMemberInP1: Boolean = false) : Seq[TaxYearResults] = {
    // Calculate results
    val inputsByTaxYear = contributions.groupBy(_.taxYearLabel)
    val allContributions = provideMissingYearContributions(contributions,
                                                           earliestYear,
                                                           missingRowsAreRegistered,
                                                           notMemberInP1)

    Logger.debug(s"""Calculating for:\n${allContributions.mkString("\n")}""")
    val results = allContributions.foldLeft(List[TaxYearResults]()) { (lst, contribution) =>
      val calculator = Calculator(contribution)

      calculator.calculate(lst, contribution) match {
        case Success((summary, details)) => TaxYearResults(contribution, summary, Some(details)) :: lst
        case Failure(e) => {
          Logger.warn(s"Calculation failed for ${contribution}")
          TaxYearResults(contribution, SummaryResult(), None) :: lst
        }
      }
    }.dropWhile(_.input.taxYearLabel > inputsByTaxYear.keys.max).toList.reverse
    val v = results.dropWhile(_.input.taxYearLabel < inputsByTaxYear.keys.min).toList
    Logger.debug(s"""Results:\n${v.mkString("\n")}""")
    val output = if (doCollate) {
      collate(v)
    } else {
      v
    }
    output
  }

  def collate(calculationResults: Seq[TaxYearResults]): Seq[TaxYearResults] = {
    val r = calculationResults.toList
    def fetchTriggered(l:List[TaxYearResults]): Option[TaxYearResults] = l.find(_.input.isTriggered)
    def fetchNotTriggered(l:List[TaxYearResults]): Option[TaxYearResults] = l.find(!_.input.isTriggered)

    r.find(_.input.isTriggered).map {
      (_) =>
      val mappedResults = calculators.internal.utilities.grouped(r)
      mappedResults.map {
        (entry)=>
        entry._1 match {
          case "2015" => {
            val year2015ResultsMap = entry._2.groupBy(_.input.isPeriod1)
            val period1Results = year2015ResultsMap.get(true).get.toList
            val period2Results = year2015ResultsMap.get(false).get.toList
            val non2015Results = r.filterNot((t)=>t.input.isPeriod1||t.input.isPeriod2)
            if (period1Results.size == 2) {
              val p1Triggered = fetchTriggered(period1Results).get.summaryResult
              val p1TriggeredDetails = fetchTriggered(period1Results).get.details
              val p1NotTriggered = fetchNotTriggered(period1Results).get
              val newP1 = TaxYearResults(p1NotTriggered.input, p1Triggered, p1TriggeredDetails)
              (List(newP1) ++ List(fetchTriggered(period2Results).get)).toIndexedSeq
            } else if (period2Results.size == 2) {
              val p2Triggered = fetchTriggered(period2Results).get.summaryResult
              val p2TriggeredDetails = fetchTriggered(period2Results).get.details
              val p2NotTriggered = fetchNotTriggered(period2Results).get
              val newP2 = TaxYearResults(p2NotTriggered.input, p2Triggered, p2TriggeredDetails)
              (List(fetchNotTriggered(period1Results).get) ++ List(newP2)).toIndexedSeq
            } else {
              entry._2
            }
          }
          case "<2015" => entry._2.toIndexedSeq
          case ">2015" => {
            entry._2.groupBy(_.input.taxPeriodStart.taxYear).map {
              (subentry)=>
              if (subentry._2.size == 2) {
                val triggered = fetchTriggered(subentry._2.toList)
                val notTriggered = fetchNotTriggered(subentry._2.toList)
                List(TaxYearResults(notTriggered.get.input,triggered.get.summaryResult,triggered.get.details)).toIndexedSeq
              } else {
                subentry._2
              }
            }.flatten.toList
          }
        }
      }.flatten.toList.sortBy(_.input.taxPeriodStart.taxYear)
    }.getOrElse(calculationResults)
  }
}

object PensionAllowanceCalculator extends PensionAllowanceCalculator
