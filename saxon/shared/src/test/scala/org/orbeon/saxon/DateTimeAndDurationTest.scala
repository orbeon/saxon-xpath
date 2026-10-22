package org.orbeon.saxon

class DateTimeAndDurationTest extends XPathTestBase {

  describe("XML Schema Date and Time Types") {

    describe("Constructor and String representation") {
      check("string(xs:date('2023-11-05'))", "2023-11-05")
      check("string(xs:date('2023-11-05Z'))", "2023-11-05Z")
      check("string(xs:date('2023-11-05+05:00'))", "2023-11-05+05:00")
      check("string(xs:date('2023-11-05-08:00'))", "2023-11-05-08:00")
      check("string(xs:date('2024-02-29'))", "2024-02-29") // leap year

      check("string(xs:time('14:30:00'))", "14:30:00")
      check("string(xs:time('14:30:00.5'))", "14:30:00.5")
      check("string(xs:time('14:30:00Z'))", "14:30:00Z")
      check("string(xs:time('14:30:00-05:00'))", "14:30:00-05:00")
      check("string(xs:time('00:00:00'))", "00:00:00")
      check("string(xs:time('24:00:00'))", "00:00:00") // 24:00:00 normalizes to 00:00:00 of next day in XPath

      check("string(xs:dateTime('2023-11-05T14:30:00'))", "2023-11-05T14:30:00")
      check("string(xs:dateTime('2023-11-05T14:30:00.123Z'))", "2023-11-05T14:30:00.123Z")
      check("string(xs:dateTime('2023-11-05T14:30:00+02:00'))", "2023-11-05T14:30:00+02:00")

      check("string(xs:duration('P1Y2M3DT4H5M6S'))", "P1Y2M3DT4H5M6S")
      check("string(xs:duration('-P1Y2M'))", "-P1Y2M")
      check("string(xs:duration('PT0S'))", "PT0S")

      check("string(xs:yearMonthDuration('P1Y2M'))", "P1Y2M")
      check("string(xs:yearMonthDuration('-P3Y'))", "-P3Y")
      check("string(xs:yearMonthDuration('P0M'))", "P0M")

      check("string(xs:dayTimeDuration('P1DT2H3M4S'))", "P1DT2H3M4S")
      check("string(xs:dayTimeDuration('PT30M'))", "PT30M")
      check("string(xs:dayTimeDuration('-PT1H'))", "-PT1H")
      check("string(xs:dayTimeDuration('PT0S'))", "PT0S")
    }

    describe("Component Extraction Functions") {

      describe("xs:date components") {
        check("year-from-date(xs:date('2023-11-05'))", "2023")
        check("month-from-date(xs:date('2023-11-05'))", "11")
        check("day-from-date(xs:date('2023-11-05'))", "5")
        check("string(timezone-from-date(xs:date('2023-11-05Z')))", "PT0S")
        check("string(timezone-from-date(xs:date('2023-11-05+05:30')))", "PT5H30M")
        check("empty(timezone-from-date(xs:date('2023-11-05')))", "true")
        check("empty(year-from-date(()))", "true")
      }

      describe("xs:time components") {
        check("hours-from-time(xs:time('14:30:45.123'))", "14")
        check("minutes-from-time(xs:time('14:30:45.123'))", "30")
        check("seconds-from-time(xs:time('14:30:45.123'))", "45.123")
        check("string(timezone-from-time(xs:time('14:30:00Z')))", "PT0S")
        check("string(timezone-from-time(xs:time('14:30:00-08:00')))", "-PT8H")
        check("empty(timezone-from-time(xs:time('14:30:00')))", "true")
        check("empty(hours-from-time(()))", "true")
      }

      describe("xs:dateTime components") {
        check("year-from-dateTime(xs:dateTime('2023-11-05T14:30:45.5Z'))", "2023")
        check("month-from-dateTime(xs:dateTime('2023-11-05T14:30:45.5Z'))", "11")
        check("day-from-dateTime(xs:dateTime('2023-11-05T14:30:45.5Z'))", "5")
        check("hours-from-dateTime(xs:dateTime('2023-11-05T14:30:45.5Z'))", "14")
        check("minutes-from-dateTime(xs:dateTime('2023-11-05T14:30:45.5Z'))", "30")
        check("seconds-from-dateTime(xs:dateTime('2023-11-05T14:30:45.5Z'))", "45.5")
        check("string(timezone-from-dateTime(xs:dateTime('2023-11-05T14:30:45.5+01:00')))", "PT1H")
        check("empty(timezone-from-dateTime(xs:dateTime('2023-11-05T14:30:45')))", "true")
        check("empty(year-from-dateTime(()))", "true")
      }

      describe("Duration components") {
        check("years-from-duration(xs:duration('P1Y2M3DT4H5M6S'))", "1")
        check("months-from-duration(xs:duration('P1Y2M3DT4H5M6S'))", "2")
        check("days-from-duration(xs:duration('P1Y2M3DT4H5M6S'))", "3")
        check("hours-from-duration(xs:duration('P1Y2M3DT4H5M6S'))", "4")
        check("minutes-from-duration(xs:duration('P1Y2M3DT4H5M6S'))", "5")
        check("seconds-from-duration(xs:duration('P1Y2M3DT4H5M6.5S'))", "6.5")

        // Negative duration components
        check("years-from-duration(xs:duration('-P1Y2M'))", "-1")
        check("months-from-duration(xs:duration('-P1Y2M'))", "-2")
        check("hours-from-duration(xs:dayTimeDuration('-PT1H30M'))", "-1")
        check("minutes-from-duration(xs:dayTimeDuration('-PT1H30M'))", "-30")

        check("empty(years-from-duration(()))", "true")
      }
    }

    describe("Timezone Adjustment Functions") {
      check(
        "string(adjust-date-to-timezone(xs:date('2023-11-05'), xs:dayTimeDuration('PT5H')))",
        "2023-11-05+05:00"
      )
      check(
        "string(adjust-date-to-timezone(xs:date('2023-11-05+02:00'), ()))",
        "2023-11-05"
      )
      check(
        "string(adjust-time-to-timezone(xs:time('12:00:00'), xs:dayTimeDuration('-PT8H')))",
        "12:00:00-08:00"
      )
      check(
        "string(adjust-time-to-timezone(xs:time('12:00:00Z'), ()))",
        "12:00:00"
      )
      check(
        "string(adjust-dateTime-to-timezone(xs:dateTime('2023-11-05T12:00:00Z'), xs:dayTimeDuration('PT2H')))",
        "2023-11-05T14:00:00+02:00"
      )
      check(
        "string(adjust-dateTime-to-timezone(xs:dateTime('2023-11-05T14:00:00+02:00'), ()))",
        "2023-11-05T14:00:00"
      )
      check("empty(adjust-date-to-timezone((), xs:dayTimeDuration('PT0S')))", "true")
    }

    describe("Current Date and Time") {
      check("current-date() instance of xs:date", "true")
      check("current-time() instance of xs:time", "true")
      check("current-dateTime() instance of xs:dateTime", "true")
      check("implicit-timezone() instance of xs:dayTimeDuration", "true")

      // Stability: within a single execution, current-dateTime() is constant
      check(
        "let $dt1 := current-dateTime(), $dt2 := current-dateTime() return $dt1 = $dt2",
        "true"
      )
    }

    describe("Date and Time Comparisons") {
      check("xs:date('2023-11-05') = xs:date('2023-11-05')", "true")
      check("xs:date('2023-11-05') < xs:date('2023-11-06')", "true")
      check("xs:date('2023-11-05') > xs:date('2023-11-04')", "true")

      check("xs:time('14:30:00') = xs:time('14:30:00')", "true")
      check("xs:time('14:30:00') < xs:time('14:30:01')", "true")

      check("xs:dateTime('2023-11-05T12:00:00Z') = xs:dateTime('2023-11-05T14:00:00+02:00')", "true")
      check("xs:dateTime('2023-11-05T12:00:00Z') < xs:dateTime('2023-11-05T13:00:00Z')", "true")

      check("xs:yearMonthDuration('P1Y') = xs:yearMonthDuration('P12M')", "true")
      check("xs:yearMonthDuration('P1Y') < xs:yearMonthDuration('P1Y1M')", "true")

      check("xs:dayTimeDuration('P1D') = xs:dayTimeDuration('PT24H')", "true")
      check("xs:dayTimeDuration('PT60M') = xs:dayTimeDuration('PT1H')", "true")
      check("xs:dayTimeDuration('PT60S') = xs:dayTimeDuration('PT1M')", "true")
      check("xs:dayTimeDuration('PT1H') < xs:dayTimeDuration('PT1H1S')", "true")
    }

    describe("Date, Time and Duration Arithmetic") {

      describe("Date +/- Duration") {
        check(
          "string(xs:date('2023-01-31') + xs:yearMonthDuration('P1M'))",
          "2023-02-28" // end-of-month pinning
        )
        check(
          "string(xs:date('2024-01-31') + xs:yearMonthDuration('P1M'))",
          "2024-02-29" // leap year
        )
        check(
          "string(xs:date('2023-11-05') + xs:yearMonthDuration('P1Y'))",
          "2024-11-05"
        )
        check(
          "string(xs:date('2023-11-05') - xs:yearMonthDuration('P1Y'))",
          "2022-11-05"
        )
        check(
          "string(xs:date('2023-11-05') + xs:dayTimeDuration('P3D'))",
          "2023-11-08"
        )
        check(
          "string(xs:date('2023-11-05') - xs:dayTimeDuration('P5D'))",
          "2023-10-31"
        )
      }

      describe("DateTime +/- Duration") {
        check(
          "string(xs:dateTime('2023-11-05T10:00:00Z') + xs:dayTimeDuration('P1DT2H30M'))",
          "2023-11-06T12:30:00Z"
        )
        check(
          "string(xs:dateTime('2023-11-05T10:00:00Z') - xs:dayTimeDuration('PT11H'))",
          "2023-11-04T23:00:00Z"
        )
        check(
          "string(xs:dateTime('2023-01-31T12:00:00') + xs:yearMonthDuration('P1M'))",
          "2023-02-28T12:00:00"
        )
      }

      describe("Time +/- Duration") {
        check(
          "string(xs:time('10:00:00') + xs:dayTimeDuration('PT2H30M'))",
          "12:30:00"
        )
        check(
          "string(xs:time('10:00:00') - xs:dayTimeDuration('PT1H'))",
          "09:00:00"
        )
        check(
          "string(xs:time('23:00:00') + xs:dayTimeDuration('PT2H'))",
          "01:00:00" // wrap around 24 hours
        )
      }

      describe("Differences (Date - Date, DateTime - DateTime)") {
        check(
          "string(xs:date('2023-11-08') - xs:date('2023-11-05'))",
          "P3D"
        )
        check(
          "string(xs:date('2023-11-01') - xs:date('2023-11-05'))",
          "-P4D"
        )
        check(
          "string(xs:dateTime('2023-11-05T14:30:00Z') - xs:dateTime('2023-11-05T10:00:00Z'))",
          "PT4H30M"
        )
        check(
          "string(xs:time('14:30:00') - xs:time('10:00:00'))",
          "PT4H30M"
        )
      }

      describe("Duration Arithmetic") {
        check(
          "string(xs:yearMonthDuration('P1Y2M') + xs:yearMonthDuration('P2Y11M'))",
          "P4Y1M"
        )
        check(
          "string(xs:yearMonthDuration('P2Y') - xs:yearMonthDuration('P8M'))",
          "P1Y4M"
        )
        check(
          "string(xs:dayTimeDuration('P1DT12H') + xs:dayTimeDuration('P2DT18H'))",
          "P4DT6H"
        )
        check(
          "string(xs:dayTimeDuration('P2D') - xs:dayTimeDuration('PT12H'))",
          "P1DT12H"
        )

        // Duration multiplied/divided by numbers
        check(
          "string(xs:yearMonthDuration('P2Y') * 2.5)",
          "P5Y"
        )
        check(
          "string(xs:yearMonthDuration('P2Y') div 2)",
          "P1Y"
        )
        check(
          "string(xs:dayTimeDuration('PT1H') * 3)",
          "PT3H"
        )
        check(
          "string(xs:dayTimeDuration('PT1H') div 2)",
          "PT30M"
        )

        // Duration divided by duration -> number
        check(
          "xs:yearMonthDuration('P3Y') div xs:yearMonthDuration('P6M')",
          "6"
        )
        check(
          "xs:dayTimeDuration('P1D') div xs:dayTimeDuration('PT6H')",
          "4"
        )
      }
    }

    describe("Date/Time Casting and Conversion") {
      check("xs:date(xs:dateTime('2023-11-05T14:30:00Z'))", "2023-11-05Z")
      check("xs:time(xs:dateTime('2023-11-05T14:30:00Z'))", "14:30:00Z")
      check(
        "string(dateTime(xs:date('2023-11-05'), xs:time('14:30:00')))",
        "2023-11-05T14:30:00"
      )
      check(
        "string(dateTime(xs:date('2023-11-05Z'), xs:time('14:30:00Z')))",
        "2023-11-05T14:30:00Z"
      )
    }
  }
}
