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

package models

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

sealed trait PensionResult
sealed trait PensionCalculationResult extends PensionResult

case class TaxYearResults(input: Contribution,
                          summaryResult: SummaryResult) extends PensionCalculationResult
case class SummaryResult(chargableAmount: Long = 0,
                         exceedingAAAmount: Long = 0,
                         availableAllowance: Long = 0,
                         unusedAllowance: Long = 0,
                         availableAAWithCF: Long = 0,    // total available allowance for current year should be renamed to totalAA
                         availableAAWithCCF: Long = 0,   // available allowance carried forward to following year
                         unusedAllowanceCF: Long = 0,
                         moneyPurchaseAA: Long = 0,
                         alternativeAA: Long = 0,
                         dbist: Long = 0,
                         mpist: Long = 0,
                         alternativeChargableAmount: Long = 0,
                         defaultChargableAmount: Long = 0) extends PensionCalculationResult

object SummaryResult {
  implicit val summaryResultWrites: Writes[SummaryResult] = (
    (JsPath \ "chargableAmount").write[Long] and
    (JsPath \ "exceedingAAAmount").write[Long] and 
    (JsPath \ "availableAllowance").write[Long] and
    (JsPath \ "unusedAllowance").write[Long] and 
    (JsPath \ "availableAAWithCF").write[Long] and
    (JsPath \ "availableAAWithCCF").write[Long] and
    (JsPath \ "unusedAllowanceCF").write[Long] and
    (JsPath \ "moneyPurchaseAA").write[Long] and
    (JsPath \ "alternativeAA").write[Long] and
    (JsPath \ "dbist").write[Long] and
    (JsPath \ "mpist").write[Long] and
    (JsPath \ "alternativeChargableAmount").write[Long] and
    (JsPath \ "defaultChargableAmount").write[Long]
  )(unlift(SummaryResult.unapply))

  implicit val summaryResultReads: Reads[SummaryResult] = (
    (JsPath \ "chargableAmount").read[Long] and
    (JsPath \ "exceedingAAAmount").read[Long] and
    (JsPath \ "availableAllowance").read[Long] and
    (JsPath \ "unusedAllowance").read[Long] and
    (JsPath \ "availableAAWithCF").read[Long] and
    (JsPath \ "availableAAWithCCF").read[Long] and
    (JsPath \ "unusedAllowanceCF").read[Long] and 
    (JsPath \ "moneyPurchaseAA").read[Long] and
    (JsPath \ "alternativeAA").read[Long] and
    (JsPath \ "dbist").read[Long] and
    (JsPath \ "mpist").read[Long] and
    (JsPath \ "alternativeChargableAmount").read[Long] and
    (JsPath \ "defaultChargableAmount").read[Long]  
  )(SummaryResult.apply _)
}

object TaxYearResults {
  implicit val summaryWrites: Writes[TaxYearResults] = (
    (JsPath \ "input").write[Contribution] and
    (JsPath \ "summaryResult").write[SummaryResult]
  )(unlift(TaxYearResults.unapply))

  implicit val summaryReads: Reads[TaxYearResults] = (
    (JsPath \ "input").read[Contribution] and
    (JsPath \ "summaryResult").read[SummaryResult]
  )(TaxYearResults.apply _)
}
