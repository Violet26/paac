/*
 * Copyright 2016 HM Revenue & Customs
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

package calculators.periods

import models._
import calculators.results.Utilities._

package object Utilities {
  def isBefore2015(taxYearResult: TaxYearResults): Boolean = !(taxYearResult.input.isPeriod1 || taxYearResult.input.isPeriod2) && taxYearResult.input.taxPeriodStart.year <= 2015
    
  def isTriggered(implicit contribution: Contribution): Boolean = contribution.isTriggered

  def taxResultNotTriggered(tx: TaxYearResults): Boolean = (tx.input.isPeriod1 || tx.input.isPeriod2) && !tx.input.amounts.getOrElse(InputAmounts()).triggered.getOrElse(false)

  def taxResultTriggered(tx: TaxYearResults): Boolean = (tx.input.isPeriod1 || tx.input.isPeriod2) && !taxResultNotTriggered(tx)

  def maybeExtended(t: TaxYearResults): Option[ExtendedSummaryFields] = if (t.summaryResult.isInstanceOf[ExtendedSummaryFields]) Some(t.summaryResult.asInstanceOf[ExtendedSummaryFields]) else None

  def notTriggered(implicit previousPeriods:Seq[TaxYearResults]): Option[TaxYearResults] = previousPeriods.find(taxResultNotTriggered)

  def preTriggerFields(implicit previousPeriods:Seq[TaxYearResults]): Option[ExtendedSummaryFields] = notTriggered.flatMap(maybeExtended(_))

  def preTriggerInputs(implicit previousPeriods:Seq[TaxYearResults]): Option[Contribution] = notTriggered.map(_.input)

  def actualUnusedAllowance(list: List[(Int,Long)])(noOfYears: Int): Long = list.slice(0,noOfYears).foldLeft(0L)(_+_._2)

  /**
  * Helper method to convert list of tax year results into a simplified tuple list in forward order (e.g. 2008, 2009, 2010) 
  * taking into consideration if the contribution is period 1 or 2
  */
  def extractValues(calculator:calculators.periods.PeriodCalculator)(p:Seq[TaxYearResults], c: Contribution): List[SummaryResultsTuple] = {
    implicit val contribution = c
    // handle period 1 and 2 separately so filter out of previous results
    val previousPeriods = p.filterNot(_.input.isTriggered).filterNot((r)=>r.input.isPeriod1||r.input.isPeriod2)

    // add back in either period 1 or 2 as the result for 2015
    val prefix = if (contribution.isPeriod1()) {
      List((2015, calculator.definedBenefit, calculator.annualAllowance, calculator.exceedingAllowance, calculator.unusedAllowance))
    } else if (contribution.isPeriod2()) {
      List((2015, calculator.definedBenefit, calculator.annualAllowance, calculator.exceedingAllowance, calculator.unusedAllowance))
    } else {
      List((contribution.taxPeriodStart.year, calculator.definedBenefit, calculator.annualAllowance, calculator.exceedingAllowance, calculator.unusedAllowance))
    }

    // build list
    val list = (prefix ++
      previousPeriods.map {
        (result) =>
          val amounts = result.input.amounts.getOrElse(InputAmounts())
          val summary = result.summaryResult
          (result.input.taxPeriodStart.year, amounts.definedBenefit.getOrElse(0L), summary.availableAllowance, summary.exceedingAAAmount, summary.unusedAllowance)
      }.toList).reverse

    // if period 2 or later there will be period 1 in the results so recalculate allowances when period 1 exceeds the allowance
    p.find(_.input.isPeriod1).map{
      (period1) =>
      val sr = period1.summaryResult
      if (sr.exceedingAAAmount > 0) {
        val l = list.filter(_._1<2015).reverse
        val newUnusedAllowances = useAllowances(sr.exceedingAAAmount, 2015, 0, sr.unusedAllowance, l).drop(1).reverse
        val (before,after) = l.reverse.splitAt(4)
        val newAfter = newUnusedAllowances.zip(after).map((t)=>(t._2._1, t._2._2, t._2._3, t._2._4, t._1._2))
        before ++ newAfter ++ list.filter(_._1>2014)
      } else {
        list
      }
    }.getOrElse(list)
  }
}
