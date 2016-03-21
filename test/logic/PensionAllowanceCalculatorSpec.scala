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

package logic

import uk.gov.hmrc.play.test.UnitSpec

import models._

import org.scalatest._
//import org.scalacheck.Prop.forAll

class PensionAllowanceCalculatorSpec extends UnitSpec {
    trait ContributionPre2014Fixture {
      val contribution0 = Contribution(2008, 500000)
      val contribution1 = Contribution(2009, 600000)
      val contribution2 = Contribution(2010, 700000)
      val contribution3 = Contribution(2011, 800000)
      val contribution4 = Contribution(2012, 900000)
      val contribution5 = Contribution(2013, 1000000)
    }

  "PensionAllowanceCalculator" should {
    "return 0 for any input after 2013 tax year" in {
      // set up
      val input = Contribution(TaxPeriod(2016, 3, 1), TaxPeriod(2016, 8, 31), InputAmounts())
      val inputs = List(input)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 1
      results(0) shouldBe TaxYearResults(input, SummaryResult())
    }

    "return 0 for contributions prior to 2008" in {
      // set up
      val input = Contribution(TaxPeriod(1914, 3, 1), TaxPeriod(1915, 8, 31), InputAmounts())
      val inputs = List(input)

      // do it
      val results = PensionAllowanceCalculator.calculateAllowances(inputs)

      // check it
      results.size shouldBe 1
      results(0) shouldBe TaxYearResults(input, SummaryResult())
    }

    "for contributions prior to 2014 and after 2008" should {
      "return correct allowances and carry forward values" in new ContributionPre2014Fixture {
        // set up
        val inputs = List(contribution0, contribution1, contribution2, contribution3, contribution4, contribution5)

        // do it
        val results = PensionAllowanceCalculator.calculateAllowances(inputs)

        // check it
        results.size shouldBe 6
        results(0) shouldBe TaxYearResults(inputs(0), SummaryResult(0,0,5000000,4500000,5000000,4500000))
        results(1) shouldBe TaxYearResults(inputs(1), SummaryResult(0,0,5000000,4400000,9500000,8900000))
        results(2) shouldBe TaxYearResults(inputs(2), SummaryResult(0,0,5000000,4300000,13900000,13200000))
        results(3) shouldBe TaxYearResults(inputs(3), SummaryResult(0,0,5000000,4200000,18200000,12900000))
        results(4) shouldBe TaxYearResults(inputs(4), SummaryResult(0,0,5000000,4100000,17900000,12600000))
        results(5) shouldBe TaxYearResults(inputs(5), SummaryResult(0,0,5000000,4000000,17600000,12300000))
      }

      "return correct allowances and carry forward values even if inputs are not in tax year sequential order" in new ContributionPre2014Fixture {
        // set up
        val inputs = List(contribution3, contribution1, contribution0, contribution2, contribution5, contribution4)

        // do it
        val results = PensionAllowanceCalculator.calculateAllowances(inputs)

        // check it
        results.size shouldBe 6
        results(0) shouldBe TaxYearResults(contribution0, SummaryResult(0,0,5000000,4500000,5000000,4500000))
        results(1) shouldBe TaxYearResults(contribution1, SummaryResult(0,0,5000000,4400000,9500000,8900000))
        results(2) shouldBe TaxYearResults(contribution2, SummaryResult(0,0,5000000,4300000,13900000,13200000))
        results(3) shouldBe TaxYearResults(contribution3, SummaryResult(0,0,5000000,4200000,18200000,12900000))
        results(4) shouldBe TaxYearResults(contribution4, SummaryResult(0,0,5000000,4100000,17900000,12600000))
        results(5) shouldBe TaxYearResults(contribution5, SummaryResult(0,0,5000000,4000000,17600000,12300000))
      }

      "return correct allowances and carry forward values if interim tax year inputs are missing" in new ContributionPre2014Fixture {
        // set up
        val inputs = List(contribution0, contribution2)

        // do it
        val results = PensionAllowanceCalculator.calculateAllowances(inputs)

        // check it
        results.size shouldBe 3
        results(0) shouldBe TaxYearResults(contribution0, SummaryResult(0,0,5000000,4500000,5000000,4500000))
        results(1) shouldBe TaxYearResults(Contribution(2009,0), SummaryResult(0,0,5000000,5000000,9500000,9500000))
        results(2) shouldBe TaxYearResults(contribution2, SummaryResult(0,0,5000000,4300000,14500000,13800000))
      }

      "return correct calculations when pension contributions are 50000" in {
        // set up
        val inputs = List(Contribution(2008, 5000000),
                          Contribution(2009, 5000000),
                          Contribution(2010, 5000000),
                          Contribution(2011, 5000000),
                          Contribution(2012, 5000000),
                          Contribution(2013, 5000000))

        // do it
        val results = PensionAllowanceCalculator.calculateAllowances(inputs)

        // check results
        results.size shouldBe 6
        results(0) shouldBe TaxYearResults(Contribution(2008, 5000000), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=0,availableAAWithCF=5000000,availableAAWithCCF=0))
        results(1) shouldBe TaxYearResults(Contribution(2009, 5000000), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=0,availableAAWithCF=5000000,availableAAWithCCF=0))
        results(2) shouldBe TaxYearResults(Contribution(2010, 5000000), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=0,availableAAWithCF=5000000,availableAAWithCCF=0))
        results(3) shouldBe TaxYearResults(Contribution(2011, 5000000), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=0,availableAAWithCF=5000000,availableAAWithCCF=0))
        results(4) shouldBe TaxYearResults(Contribution(2012, 5000000), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=0,availableAAWithCF=5000000,availableAAWithCCF=0))
        results(5) shouldBe TaxYearResults(Contribution(2013, 5000000), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=0,availableAAWithCF=5000000,availableAAWithCCF=0))
      }

      "return correct calculations when pension contributions are less than 50000" in {
        // set up
        val inputs = List(Contribution(2008, 4000000),
                          Contribution(2009, 4000000),
                          Contribution(2010, 4000000),
                          Contribution(2011, 4000000),
                          Contribution(2012, 4000000),
                          Contribution(2013, 4000000))

        // do it
        val results = PensionAllowanceCalculator.calculateAllowances(inputs)

        // check results
        results.size shouldBe 6
        results(0) shouldBe TaxYearResults(Contribution(2008, 4000000), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=1000000,availableAAWithCF=5000000,availableAAWithCCF=1000000))
        results(1) shouldBe TaxYearResults(Contribution(2009, 4000000), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=1000000,availableAAWithCF=6000000,availableAAWithCCF=2000000))
        results(2) shouldBe TaxYearResults(Contribution(2010, 4000000), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=1000000,availableAAWithCF=7000000,availableAAWithCCF=3000000))
        results(3) shouldBe TaxYearResults(Contribution(2011, 4000000), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=1000000,availableAAWithCF=8000000,availableAAWithCCF=3000000))
        results(4) shouldBe TaxYearResults(Contribution(2012, 4000000), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=1000000,availableAAWithCF=8000000,availableAAWithCCF=3000000))
        results(5) shouldBe TaxYearResults(Contribution(2013, 4000000), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=1000000,availableAAWithCF=8000000,availableAAWithCCF=3000000))
      }

      "return correct calculations when pension contributions are greater than 50000" in {
        // set up
        val inputs = List(Contribution(2008, 6000000),
                          Contribution(2009, 6000000),
                          Contribution(2010, 6000000),
                          Contribution(2011, 6000000),
                          Contribution(2012, 6000000),
                          Contribution(2013, 6000000))

        // do it
        val results = PensionAllowanceCalculator.calculateAllowances(inputs)

        // check results
        results.size shouldBe 6
        results(0) shouldBe TaxYearResults(Contribution(2008, 6000000), SummaryResult(chargableAmount=0,      exceedingAAAmount=1000000,availableAllowance=5000000,unusedAllowance=0,availableAAWithCF=5000000,availableAAWithCCF=0))
        results(1) shouldBe TaxYearResults(Contribution(2009, 6000000), SummaryResult(chargableAmount=0,      exceedingAAAmount=1000000,availableAllowance=5000000,unusedAllowance=0,availableAAWithCF=5000000,availableAAWithCCF=0))
        results(2) shouldBe TaxYearResults(Contribution(2010, 6000000), SummaryResult(chargableAmount=0,      exceedingAAAmount=1000000,availableAllowance=5000000,unusedAllowance=0,availableAAWithCF=5000000,availableAAWithCCF=0))
        results(3) shouldBe TaxYearResults(Contribution(2011, 6000000), SummaryResult(chargableAmount=1000000,exceedingAAAmount=1000000,availableAllowance=5000000,unusedAllowance=0,availableAAWithCF=5000000,availableAAWithCCF=0))
        results(4) shouldBe TaxYearResults(Contribution(2012, 6000000), SummaryResult(chargableAmount=1000000,exceedingAAAmount=1000000,availableAllowance=5000000,unusedAllowance=0,availableAAWithCF=5000000,availableAAWithCCF=0))
        results(5) shouldBe TaxYearResults(Contribution(2013, 6000000), SummaryResult(chargableAmount=1000000,exceedingAAAmount=1000000,availableAllowance=5000000,unusedAllowance=0,availableAAWithCF=5000000,availableAAWithCCF=0))
      }

      "return correct calculations when pension contributions are 0" in {
        // set up
        val inputs = List(Contribution(2008, 0),
                          Contribution(2009, 0),
                          Contribution(2010, 0),
                          Contribution(2011, 0),
                          Contribution(2012, 0),
                          Contribution(2013, 0))

        // do it
        val results = PensionAllowanceCalculator.calculateAllowances(inputs)

        // check results
        results.size shouldBe 6
        results(0) shouldBe TaxYearResults(Contribution(2008, 0), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=5000000,availableAAWithCF=5000000 ,availableAAWithCCF=5000000 ))
        results(1) shouldBe TaxYearResults(Contribution(2009, 0), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=5000000,availableAAWithCF=10000000,availableAAWithCCF=10000000))
        results(2) shouldBe TaxYearResults(Contribution(2010, 0), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=5000000,availableAAWithCF=15000000,availableAAWithCCF=15000000))
        results(3) shouldBe TaxYearResults(Contribution(2011, 0), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=5000000,availableAAWithCF=20000000,availableAAWithCCF=15000000))
        results(4) shouldBe TaxYearResults(Contribution(2012, 0), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=5000000,availableAAWithCF=20000000,availableAAWithCCF=15000000))
        results(5) shouldBe TaxYearResults(Contribution(2013, 0), SummaryResult(chargableAmount=0,exceedingAAAmount=0,availableAllowance=5000000,unusedAllowance=5000000,availableAAWithCF=20000000,availableAAWithCCF=15000000))
      }

      "return correct calculations when pension contributions are variable amounts above and below allowance" in {
        // set up
        val inputs = List(Contribution(2008, 0),
                          Contribution(2009, 5000000),
                          Contribution(2010, 6000000),
                          Contribution(2011, 15000000),
                          Contribution(2012, 4000000),
                          Contribution(2013, 5000000))

        // do it
        val results = PensionAllowanceCalculator.calculateAllowances(inputs)

        // check results
        results.size shouldBe 6
        results(0) shouldBe TaxYearResults(Contribution(2008, 0),       SummaryResult(chargableAmount=0,       exceedingAAAmount=0,        availableAllowance=5000000, unusedAllowance=5000000, availableAAWithCF=5000000,  availableAAWithCCF=5000000))
        results(1) shouldBe TaxYearResults(Contribution(2009, 5000000), SummaryResult(chargableAmount=0,       exceedingAAAmount=0,        availableAllowance=5000000, unusedAllowance=0,       availableAAWithCF=10000000, availableAAWithCCF=5000000))
        results(2) shouldBe TaxYearResults(Contribution(2010, 6000000), SummaryResult(chargableAmount=0,       exceedingAAAmount=1000000,  availableAllowance=5000000, unusedAllowance=0,       availableAAWithCF=10000000, availableAAWithCCF=5000000))
        results(3) shouldBe TaxYearResults(Contribution(2011, 15000000),SummaryResult(chargableAmount=5000000, exceedingAAAmount=10000000, availableAllowance=5000000, unusedAllowance=0,       availableAAWithCF=10000000, availableAAWithCCF=0))
        results(4) shouldBe TaxYearResults(Contribution(2012, 4000000), SummaryResult(chargableAmount=0,       exceedingAAAmount=0,        availableAllowance=5000000, unusedAllowance=1000000, availableAAWithCF=5000000,  availableAAWithCCF=1000000))
        results(5) shouldBe TaxYearResults(Contribution(2013, 5000000), SummaryResult(chargableAmount=0,       exceedingAAAmount=0,        availableAllowance=5000000, unusedAllowance=0,       availableAAWithCF=6000000,  availableAAWithCCF=1000000))
      }
    }
  }

  "Pre2014Calculator" should {
    trait ZeroContributionFixture {
      val contribution = Contribution(TaxPeriod(2009, 3, 1), TaxPeriod(2009, 8, 31), InputAmounts())
    }

    "return none for contributions prior to 2008" in {
      // set up
      val input = Contribution(TaxPeriod(1914, 3, 1), TaxPeriod(1915, 8, 31), InputAmounts())

      // do it
      val results = Pre2014Calculator.summary(Seq[SummaryResult](), input)

      // check it
      results shouldBe None
    }

    "return some for contributions prior to 2014 and after 2007" in new ZeroContributionFixture {
      // do it
      val results = Pre2014Calculator.summary(Seq[SummaryResult](), contribution)

      // check it
      results shouldBe Some(SummaryResult(0,0,5000000,5000000,5000000,5000000))
    }

    "return amount exceeding Annual Allowance of 0 for values under 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), contribution1).get

      // check it
      result.exceedingAAAmount shouldBe 0
    }

    "return amount chargable amount of 0 for values under 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), contribution1).get

      // check it
      result.chargableAmount shouldBe 0
    }

    "return amount chargable amount of non-0 for values over 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](SummaryResult(),SummaryResult(),SummaryResult()), Contribution(2011, 5500000)).get

      // check it
      result.chargableAmount shouldBe 500000
    }

    "return amount exceeding Annual Allowance of non-0 for values over 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](SummaryResult(),SummaryResult(),SummaryResult()), Contribution(2011, 5500000)).get

      // check it
      result.exceedingAAAmount shouldBe 500000
    }

    "return available allowance of 5000000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), contribution1).get

      // check it
      result.availableAllowance shouldBe 5000000
    }

    "return unused allowance of 4400000" in new ContributionPre2014Fixture {
      // do it
      val result = Pre2014Calculator.summary(Seq[SummaryResult](), contribution1).get

      // check it
      result.unusedAllowance shouldBe 4400000
    }

    "return available allowance with carry forward of 95000" in new ContributionPre2014Fixture {
      // do it
      val starting = SummaryResult(chargableAmount = 0,
                                   exceedingAAAmount = 0,
                                   availableAllowance = 5000000,
                                   unusedAllowance = 4500000,
                                   availableAAWithCF = 5000000,
                                   availableAAWithCCF = 4500000)
      val result = Pre2014Calculator.summary(Seq[SummaryResult](starting), contribution1).get

      // check it
      result.chargableAmount shouldBe 0
      result.exceedingAAAmount shouldBe 0
      result.availableAllowance shouldBe 5000000
      result.unusedAllowance shouldBe 4400000
      result.availableAAWithCF shouldBe 9500000
      result.availableAAWithCCF shouldBe 8900000
    }
  }
} 