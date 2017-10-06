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

package calculators.internal

import calculators._
import calculators.internal.utilities._
import models._
import config.PaacConfiguration
import play.api.Logger
import scala.util._

// scalastyle:off number.of.methods
trait TaperedAllowanceCalculator extends ExtendedSummaryCalculator with DetailsCalculator {
  private val DEFAULT_MPA = 10000
  private val DEFAULT_TAA = 10000
  private val DEAFULT_TAPER_START = 150000
  private val DEAFULT_TAPER_END = 210000
  private val DEAFULT_AA = 40000

  def previousPeriods(): Seq[TaxYearResults]

  def contribution(): Contribution

  override def acaCF(): Long = _acaCF

  override def allowance(): Long = _annualAllowance

  override def alternativeAA(): Long = _alternativeAA

  override def alternativeChargableAmount(): Long = _alternativeChargableAmount

  override def annualAllowance(): Long = _taperedAllowance

  override def annualAllowanceCF(): Long = _annualAllowanceCF

  override def annualAllowanceCCF(): Long = _annualAllowanceCCF

  override def availableAAAWithCF(): Long = _alternativeAACF

  override def availableAAAWithCCF(): Long = _annualAllowanceCCF

  override def chargableAmount(): Long = _chargableAmount

  override def cumulativeDB(): Long = _cumulativeDB

  override def cumulativeMP(): Long = _cumulativeMP

  override def dbist(): Long = _dbist

  override def dcaCF(): Long = _dcaCF

  override def defaultChargableAmount(): Long = _defaultChargableAmount

  override def definedBenefit(): Long = _definedBenefit

  override def definedContribution(): Long = _definedContribution

  override def exceedingAAA(): Long = _exceedingAAA

  override def exceedingAllowance(): Long = _exceedingAllowance

  override def exceedingMPAA(): Long = _exceedingMPAA

  override def isACA(): Boolean = _isACA

  override def isMPAAApplicable(): Boolean = _isMPAAApplicable

  override def moneyPurchaseAA(): Long = _mpa

  override def mpist(): Long = _mpist

  override def postFlexiSavings(): Long = _postFlexiSavings

  override def preFlexiSavings(): Long = _preTriggerSavings

  override def unusedAAA(): Long = _unusedAAA

  override def unusedAllowance(): Long = _unusedAllowance

  override def unusedMPAA(): Long = _unusedMPAA

  protected def year = contribution.taxPeriodStart.taxYear

  protected lazy val income: Long =
    if (isTriggered) {
      previousPeriods.headOption.map {
        (previous) =>
          if (previous.input.isTriggered) {
            contribution.income
          } else {
            previous.input.income
          }
      }.getOrElse(contribution.income)
    }
    else {
      contribution.income
    }

  protected def isTaperingApplicable(): Boolean = income > _taperStart

  protected def isTriggered(): Boolean = contribution.isTriggered

  protected lazy val actualUnused = actualUnusedList(this)(previousPeriods, contribution)

  protected lazy val actualAAAUnused = actualAAAUnusedList(this)(previousPeriods, contribution)

  protected lazy val config: Map[String, Int] = PaacConfiguration.forYear(year)

  protected lazy val previousYear = previousPeriods.find(isYear(year - 1))

  protected lazy val previous3YearsUnusedAllowanceList: List[YearActualUnusedPair] = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val c = Contribution(year, Some(InputAmounts(0L, 0L)))
    val pp = previousPeriods.dropWhile(_._1 >= year)
    val calc = BasicAllowanceCalculator(0, pp, c)
    val unused = actualUnusedList(calc)(pp, c).dropWhile(_._1 == year).slice(0, 3)
    unused
  }

  protected lazy val previous3YearsUnusedAllowance: Long = previous3YearsUnusedAllowanceList.foldLeft(0L)(_ + _._2)

  protected lazy val previous3YearsUnusedAAAllowanceList: List[YearActualUnusedPair] = {
    // we only want previous values so create dummy contribution which does not affect the calculation
    val c = Contribution(year, Some(InputAmounts(0L, 0L)))
    val pp = previousPeriods.dropWhile(_._1 >= year)
    val calc = BasicAllowanceCalculator(0, pp, c)
    val unused = actualAAAUnused.dropWhile(_._1 == year).slice(0, 3)
    unused
  }

  protected lazy val previous3YearsUnusedAAAllowance: Long =
    previous3YearsUnusedAAAllowanceList.foldLeft(0L)(_ + _._2) + _previousAvailableAAWithCCF

  protected lazy val _acaCF = {
    val previousACACF = previousYear.flatMap(maybeExtended(_).map(_.acaCF)).getOrElse(0L)
    Logger.debug(s"""$year ACACF: ${alternativeChargableAmount} +  ${previousACACF}""")
    alternativeChargableAmount + previousACACF
  }

  protected lazy val _dcaCF = {
    val previousDCACF = previousYear.flatMap(maybeExtended(_).map(_.dcaCF)).getOrElse(0L)
    Logger.debug(s"""$year DCACF: ${defaultChargableAmount} +  ${previousDCACF}""")
    defaultChargableAmount + previousDCACF
  }

  protected lazy val _postFlexiSavings =
    if (isTriggered) {
      val v = contribution.moneyPurchase + contribution.definedBenefit
      detail("afs.calculation", s"mp:${currency(contribution.moneyPurchase)};op:+;db:${currency(contribution.definedBenefit)};")
      detail("afs.calculation.reason", "te")
      v
    }
    else {
      detail("afs.calculation", "ats:0")
      detail("afs.calculation.reason", "na")
      0L
    }

  protected lazy val _cumulativeMP =
    (for {
      prev <- previousYear
      prevResults <- maybeExtended(prev)
    } yield (definedContribution + prevResults.cumulativeMP)).getOrElse(definedContribution)

  protected lazy val _cumulativeDB =
    (for {
      prev <- previousYear
      prevResults <- maybeExtended(prev)
    } yield (definedBenefit + prevResults.cumulativeDB)).getOrElse(definedBenefit)

  protected lazy val _exceedingMPAA = (definedContribution - moneyPurchaseAA).max(0L)

  protected lazy val _exceedingAAA = definedBenefit - alternativeAA

  protected lazy val _isPreviousTriggered: Boolean =
    previousPeriods.headOption.map {
      (previous) =>
        previous.input.isTriggered
    }.getOrElse(false)


  protected lazy val _defaultChargableAmount =
    if (!isTriggered) {
      detail("dca.calculation", "dca:0;")
      detail("dca.calculation.reason", "nte")
      0L
    }
    else {
      if (_isPreviousTriggered) {
        val aaaa = annualAllowance + _previousAvailableAAWithCCF
        val v = (postFlexiSavings - aaaa).max(0L)
        detail("dca.calculation", s"cs:${currency(postFlexiSavings)};op: - ;aaaa:${currency(aaaa)};")
        detail("dca.calculation.reason", "pte")
        v
      } else {
        val aaaa = annualAllowance + _previousAvailableAAWithCCF
        val v = ((postFlexiSavings + preFlexiSavings) - aaaa).max(0L)
        detail("dca.calculation", s"cs:${currency(postFlexiSavings)};op: - ;aaaa:${currency(aaaa)};")
        detail("dca.calculation.reason", "te")
        v
      }
    }

  protected def unusedAllowancesList: List[Long] = {
    val (v1, v2, v3) = unusedAllowances
    List(v1, v2, v3)
  }

  protected def unusedAllowances: (Long, Long, Long) = {
    val unused = previous3YearsUnusedAllowanceList
    val cyMinus1 = Try(unused(0)).getOrElse((0, 0L))._2
    val cyMinus2 = Try(unused(1)).getOrElse((0, 0L))._2
    val cyMinus3 = Try(unused(2)).getOrElse((0, 0L))._2
    (cyMinus1, cyMinus2, cyMinus3)
  }

  protected def unusedAAAllowancesList: List[Long] = {
    val (v1, v2, v3) = unusedAAAllowances
    List(v1, v2, v3)
  }

  protected def unusedAAAllowances: (Long, Long, Long) = {
    val unused = previous3YearsUnusedAAAllowanceList
    val cyMinus1 = Try(unused(0)).getOrElse((0, 0L))._2
    val cyMinus2 = Try(unused(1)).getOrElse((0, 0L))._2
    val cyMinus3 = Try(unused(2)).getOrElse((0, 0L))._2
    (cyMinus1, cyMinus2, cyMinus3)
  }

  protected def unusedAllowancesCY: Long = {
    val (cyMinus1, cyMinus2, cyMinus3) = unusedAllowances
    detail("allowance.ccf.calculation",
      s"cyunused:${currency((annualAllowance - definedBenefit).max(0))};op:+;" +
        s"unused_${year - 1}:${currency(cyMinus1)};op:+;unused_${year - 2}:${currency(cyMinus2)};")
    detail("allowance.ccf.calculation.reason", "nte_3")
    cyMinus1 + cyMinus2 + (annualAllowance - definedBenefit)
  }

  protected def unusedAllowancesCY1And2: Long = {
    val (cyMinus1, cyMinus2, cyMinus3) = unusedAllowances
    detail("allowance.ccf.calculation", s"cyunused:0;op:+;unused_${year - 1}:${currency(cyMinus1)};op:+;unused_${year - 2}:${currency(cyMinus2)};")
    detail("allowance.ccf.calculation.reason", "nte_2")
    cyMinus1 + cyMinus2
  }

  protected def unusedAllowancesReducedCY3: Long = {
    val (cyMinus1, cyMinus2, cyMinus3) = unusedAllowances
    val v2 = (cyMinus1 + cyMinus2 + (cyMinus3 - unusedExcess)).max(0)
    if ((cyMinus2 + (cyMinus3 - unusedExcess)) < 0) {
      detail("allowance.ccf.calculation",
        s"cyunused:0;op:+;unused_${year - 1}:${currency(v2)};op:+;unused_${year - 2}:${currency(0)};")
    } else {
      detail("allowance.ccf.calculation",
        s"cyunused:0;op: + ;unused_${year - 1}:${currency(cyMinus1)};" +
          s"op: + ;unused_${year - 2}:${currency(cyMinus2 + (cyMinus3 - unusedExcess))};")
    }
    detail("allowance.ccf.calculation.reason", "nte_1")
    v2
  }

  protected def unusedAllowancesNotMPA: Long = {
    val (cyMinus1, cyMinus2, cyMinus3) = unusedAllowances
    val thisYearUnused = (_annualAllowanceCF - (preFlexiSavings + definedContribution) - (cyMinus1 + cyMinus2 + cyMinus3)).max(0)
    val v = thisYearUnused + cyMinus1 + cyMinus2
    detail("allowance.ccf.calculation",
      s"cyunused:${currency(thisYearUnused)};op: + ;" +
        s"unused_${year - 1}:${currency(cyMinus1)};op:+;unused_${year - 2}:${currency(cyMinus2)}")
    detail("allowance.ccf.calculation.reason", "aca_nte")
    v
  }

  protected lazy val previous3YearsIsACA: List[Boolean] =
    previousPeriods.dropWhile(_._1 >= year).slice(0, 3).map {
      _.summaryResult match {
        case e: ExtendedSummaryFields if e.isACA => true
        case _ => false
      }
    }.toList

  protected def unusedAllowancesMPA: Long = {
    def thisYearUnusedAllowance: Long =
      actualAAAUnused.headOption.map(_._2).getOrElse(0L)

    def previousUnusedAllowances: List[Long] =
      unusedAllowancesList
        .zip(unusedAAAllowancesList)
        .zip(previous3YearsIsACA)
        .map(p => if (p._2) p._1._2 else p._1._1)

    val (cyMinus1: Long, cyMinus2: Long) = (previousUnusedAllowances(0), previousUnusedAllowances(1))
    val prev = previousUnusedAllowances.slice(0, 3).foldLeft(0L)(_ + _)
    // Oct 2017  changes
    val v =
      if(year == 2016 && cyMinus1==0 && previousYear.get.summaryResult.unusedAllowance!=0){
        Logger.debug("unusedAllowancesMPA Example3 option 1 : "+(previousYear.get.summaryResult.unusedAllowance + prev - cyMinus2))
        Logger.debug("unusedAllowancesMPA Example3 option 2 : "+ (unusedAllowancesReducedCY3 - moneyPurchaseAA))
        previousYear.get.summaryResult.unusedAllowance + prev - cyMinus2
        //unusedAllowancesReducedCY3 - moneyPurchaseAA
      }
      else if (year == 2017 || (thisYearUnusedAllowance + cyMinus1 + cyMinus2) != 0) {
        thisYearUnusedAllowance + cyMinus1 + cyMinus2
      } else {
         previousYear.get.summaryResult.unusedAAA + prev
      }
    //val v = thisYearUnusedAllowance + cyMinus1 + cyMinus2// org

    detail("allowance.ccf.calculation",
      s"cyunused:${currency(thisYearUnusedAllowance)};op: + ;" +
        s"unused_${year - 1}:${currency(cyMinus1)};op: + ;unused_${year - 2}:${currency(cyMinus2)};")
    v
  }

  protected def unusedExcess: Long = definedBenefit - annualAllowance

  protected def acaApplies: Boolean = alternativeChargableAmount >= defaultChargableAmount

  protected def savingsExceed: Boolean = annualAllowance - definedBenefit < 0

  protected def cy3Reduced: Boolean = unusedAllowances._3 - unusedExcess < 0

  protected def cyUnused: Boolean = _unusedAllowance > 0

  protected lazy val _annualAllowanceCCF =
    isTriggered match {
      case false if (savingsExceed && cy3Reduced) => unusedAllowancesReducedCY3
      case false if savingsExceed => unusedAllowancesCY1And2
      case false => unusedAllowancesCY
      case true if (acaApplies && isMPAAApplicable) => unusedAllowancesMPA
      case true if acaApplies => unusedAllowancesNotMPA
      case true if cyUnused => {
        detail("allowance.ccf.calculation", detail("allowance.unused.cy.calculation"))
        detail("allowance.ccf.calculation.reason", "dca_1")
        _unusedAllowance
      }
      case _ => {
        detail("allowance.ccf.calculation", detail("allowance.unused.cy.calculation") + "op: - ;" + detail("xa.calculation"))
        detail("allowance.ccf.calculation.reason", "dca_2")
        (_unusedAllowance - _exceedingAllowance).max(0L)
      }
    }

  protected lazy val _alternativeAACF = {
    def previousUnusedAllowances: List[Long] =
      previous3YearsUnusedAllowanceList
        .map(_._2)
        .zip(previous3YearsUnusedAAAllowanceList.map(_._2))
        .zip(previous3YearsIsACA)
        .map(p => if (p._2) p._1._2 else p._1._1)

    val prev = previousUnusedAllowances.slice(0, 3).foldLeft(0L)(_ + _)

    //val v = _alternativeAA + prev //org Oct 2017 changes
    val availableAAWithCCF = if (previousYear == None) prev else previousYear.get.summaryResult.availableAAWithCCF
    val v = _alternativeAA + availableAAWithCCF

    detail("allowance.alt.cf.calculation", s"aaa:${currency(alternativeAA)};op:+;aaccf:${currency(prev)}")
    detail("allowance.alt.cf.calculation.reason", "post_2018")
    v
  }

  protected lazy val _annualAllowanceCF = {
    val aa = annualAllowance()
    val acf = previousYear.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)
    detail("allowance.cf.calculation", detail("aa.calculation") + s"op:+;aaccf:${currency(acf)};")
    aa + acf
  }

  protected lazy val _unusedAllowance =
    if (isTriggered) {
      if (defaultChargableAmount >= alternativeChargableAmount) {
        val v = (annualAllowance - (preFlexiSavings + definedContribution)).max(0L)
        detail("allowance.unused.cy.calculation", detail("aa.calculation") + "op:-;" + detail("pfs.calculation") + "op:+;mp:${currency(definedContribution)};")
        detail("allowance.unused.cy.calculation.reason", "dca")
        v
      }
      else {
        val v = unusedAAA
        detail("allowance.unused.cy.calculation", detail("unusedaaa.calculation"))
        detail("allowance.unused.cy.calculation.reason", "aca")
        v
      }
    } else {
      val v = (annualAllowance - definedBenefit).max(0)
      detail("allowance.unused.cy.calculation", detail("aa.calculation") + s"op:-;db:${currency(definedBenefit)};")
      detail("allowance.unused.cy.calculation.reason", "nte")
      v
    }

  protected lazy val _exceedingAllowance =
    if (alternativeChargableAmount >= defaultChargableAmount) {
      if (isTriggered) {
        val v = ((preFlexiSavings + definedContribution) - annualAllowance).max(0L)
        detail("xa.calculation", detail("pfs.calculation") + s"op: + ;mp:${currency(definedContribution)};op:-;" + detail("aa.calculation"))
        detail("xa.calculation.reason", "aca_te")
        v
      } else {
        val v = ((definedBenefit + definedContribution) - annualAllowance).max(0L)
        detail("xa.calculation", s"db:${currency(definedBenefit)};op: + ;mp:${currency(definedContribution)};op:-;" + detail("aa.calculation"))
        detail("xa.calculation.reason", "aca_nte")
        v
      }
    }
    else {
      if (isTriggered) {
        val v = 0L
        detail("xa.calculation", "0;")
        detail("xa.calculation.reason", "dca")
        v
      } else {
        val v = (preFlexiSavings - annualAllowance).max(0L)
        detail("xa.calculation", detail("pfs.calculation") + "op:-;" + detail("aa.calculation"))
        v
      }
    }

  // taper automatically applied as part of annualAllowance calculation
  protected lazy val _alternativeAA =
    if (isMPAAApplicable) {
      val v = (annualAllowance() - moneyPurchaseAA).max(0L)
      detail("aaa.calculation", detail("aa.calculation") + s"op:-;mpa:${currency(moneyPurchaseAA)};")
      v
    } else {
      detail("aaa.calculation", "0")
      detail("aaa.calculation.reason", "nte")
      0L
    }

  protected lazy val _definedBenefit =
    if (isTriggered) {
      detail("_db.calculation", s"db:${currency(contribution.definedBenefit)};op: + ;" + detail("pfs.calculation"))
      detail("_db.calculation.reason", "te")
      contribution.definedBenefit + preFlexiSavings
    }
    else {
      detail("_db.calculation", s"db:${currency(contribution.definedBenefit)};op:+;mp:${currency(contribution.moneyPurchase)};")
      detail("_db.calculation.reason", "nte")
      contribution.definedBenefit + contribution.moneyPurchase
    }

  protected lazy val _definedContribution = contribution.moneyPurchase

  protected lazy val _chargableAmount = {
    if (!isTriggered) {
      val v = (_definedBenefit - annualAllowanceCF()).max(0)
      detail("chargable.calculation", detail("_db.calculation") + s"op:-;unusedcf:${currency(annualAllowanceCF())};")
      detail("chargable.calculation.reason", "nte")
      v
    } else if (isMPAAApplicable) {
      if (alternativeChargableAmount > defaultChargableAmount) {
        detail("chargable.calculation", detail("aca.calculation"))
      } else {
        detail("chargable.calculation", detail("dca.calculation"))
      }
      detail("chargable.calculation.reason", "mpa")
      alternativeChargableAmount.max(defaultChargableAmount)
    } else {
      detail("chargable.calculation", detail("dca.calculation"))
      detail("chargable.calculation.reason", "mpa_na")
      defaultChargableAmount
    }
  }

  protected def taperReduction(income: Long): Double =
    Math.floor(((income - _taperStart) / 100L) / 2D) * 100L

  protected lazy val _taperedAllowance =
    if (isTaperingApplicable) {
      income match {
        case i if i > _taperStart && i < _taperEnd => {
          val v = (_annualAllowance - taperReduction(i)).toLong
          detail("aa.calculation",
            s"aa:${currency(_annualAllowance)};op: - ;" +
              s"taper:${currency(taperReduction(i).toLong)};")
          detail("aa.calculation.reason", "taper")
          v
        }
        case i if i >= _taperEnd => {
          detail("aa.calculation",
            s"aa:${currency(_annualAllowance)};op: - ;" +
              s"taper:${currency(_taa)};")
          detail("aa.calculation.reason", "taper")
          _taa
        }
        case _ => {
          detail("aa.calculation", s"aa:${currency(_annualAllowance)};")
          detail("aa.calculation.reason", "no_taper")
          _annualAllowance
        }
      }
    } else {
      detail("aa.calculation", s"aa:${currency(_annualAllowance)};")
      detail("aa.calculation.reason", "no_taper")
      _annualAllowance
    }

  protected lazy val _alternativeChargableAmount =
    if (isMPAAApplicable && isTriggered) {
      val v = mpist + dbist
      detail("aca.calculation", "sep:(;" + detail("mpist.calculation") + "sep:);op: + ;sep:(;" + detail("dbist.calculation") + "sep:);")
      detail("aca.calculation.reason", "mpa")
      v
    }
    else {
      detail("aca.calculation", s"aca:0;")
      detail("aca.calculation.reason", "nte")
      0L
    }

  protected lazy val _isMPAAApplicable = {
    Logger.debug(s"$year isMPA: ${isTriggered} && ${definedContribution} > ${moneyPurchaseAA} = ${isTriggered && (definedContribution > moneyPurchaseAA)}")
    isTriggered && definedContribution > moneyPurchaseAA
  }

  protected lazy val _mpa = config.get("mpaa").getOrElse(DEFAULT_MPA) * 100L

  protected lazy val _preTriggerSavings =
    previousPeriods.find {
      (r) =>
        (r.input.taxPeriodStart.taxYear == contribution.taxPeriodStart.taxYear) && (r.input.isTriggered == false)
    }.map {
      (r) =>
        val v = r.input.definedBenefit + r.input.moneyPurchase
        detail("pfs.calculation", s"sep:(;db:${currency(r.input.definedBenefit)};op: + ;mp:${currency(r.input.moneyPurchase)};sep:);")
        v
    }.getOrElse {
      if (!isTriggered) {
        detail("pfs.calculation", detail("_definedBenefit"))
        detail("pfs.calculation.reason", "nte")
        _definedBenefit
      } else {
        detail("pfs.calculation", "pfs:0;")
        detail("pfs.calculation.reason", "na")
        0L
      }
    }

  protected lazy val _unusedMPAA =
    if (isTriggered && !isMPAAApplicable) {
      val v = moneyPurchaseAA - definedContribution
      detail("unusedmpa.calculation", s"mpa:${currency(moneyPurchaseAA)};op:-;mp:${currency(definedContribution)};")
      v
    } else {
      detail("unusedmpa.calculation", s"unusedmpa:0;")
      detail("unusedmpa.calculation.reason", "na")
      0L
    }

  protected lazy val _unusedAAA =
    if (isMPAAApplicable) {
      val v = (alternativeAA - definedBenefit).max(0)
      detail("unusedaaa.calculation", s"aaa:${currency(alternativeAA)};op:-;db:${currency(definedBenefit)};")
      v
    } else {
      detail("unusedaaa.calculation", s"unusedaaa:0;")
      detail("unusedaaa.calculation.reason", "na")
      0L
    }

  protected lazy val _previousAvailableAAWithCCF =
    previousYear.map(_.summaryResult.availableAAWithCCF).getOrElse(0L)

  protected lazy val _dbist =
    if (isMPAAApplicable) {
      val aa = alternativeAA + _previousAvailableAAWithCCF
      val v = (definedBenefit - aa).max(0)
      detail("dbist.calculation", s"sep:(;pfs:${currency(definedBenefit)};op: - ;aaaa:${currency(aa)};sep:);")
      v
    } else {
      val v = (preFlexiSavings - _previousAvailableAAWithCCF).max(0)
      detail("dbist.calculation", detail("pfs.calculation") + s"op: - ;aaccf:${currency(_previousAvailableAAWithCCF)};")
      v
    }

  protected lazy val _mpist =
    if (isMPAAApplicable) {
      val v = (definedContribution - moneyPurchaseAA).max(0)
      detail("mpist.calculation", s"ta:${currency(definedContribution)};op: - ;mpaa:${currency(moneyPurchaseAA)};")
      v
    } else {
      detail("mpist.calculation", s"mpist:0;")
      detail("mpist.calculation.reason", "na")
      0L
    }

  protected lazy val _taa = config.get("taa").getOrElse(DEFAULT_TAA) * 100L

  protected lazy val _taperStart = config.get("taperStart").getOrElse(DEAFULT_TAPER_START) * 100L

  protected lazy val _taperEnd = config.get("taperEnd").getOrElse(DEAFULT_TAPER_END) * 100L

  protected lazy val _annualAllowance: Long = config.get("annual").getOrElse(DEAFULT_AA) * 100L

  // Is ACA Applicable
  protected lazy val _isACA = isTriggered && acaApplies
}

// scalastyle:on number.of.methods

case class Post2015TaperedAllowanceCalculator(implicit previousPeriods: Seq[TaxYearResults],
                                              contribution: Contribution)
  extends TaperedAllowanceCalculator {
}
