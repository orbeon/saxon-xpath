//package org.orbeon.saxon.expr.number
//
//// ORBEON: `java.util.TimeZone` is not supported by Scala.js. Should use `java.time`.
//import java.util.TimeZone
//import java.{util => ju}
//
//import org.orbeon.saxon.tree.util.FastStringBuffer
//import org.orbeon.saxon.value.{CalendarValue, DateTimeValue}
//
////import scala.collection.compat._
//import scala.jdk.CollectionConverters._
//import scala.util.control.Breaks._
//
//
//object NamedTimeZone {
//
//  private var knownTimeZones: ju.Collection[String] = new ju.HashSet[String](50)
//
//  private var idForCountry: ju.HashMap[String, ju.List[String]] =
//    new ju.HashMap[String, ju.List[String]](50)
//
//  private var worldTimeZones: ju.List[String] = new ju.ArrayList(20)
//
//  ju.Collections.addAll(knownTimeZones, ju.TimeZone.getAvailableIDs: _*)
//
//  private def tz(country: String, zoneId: String): Unit = {
//    var list: ju.List[String] = idForCountry.get(country)
//    if (list == null) {
//      list = new ju.ArrayList[String](4)
//    }
//    list.add(zoneId)
//    idForCountry.put(country, list)
//  }
//
//  private def tz(country: String, zoneId: String, major: Boolean): Unit = {
//    tz(country, zoneId)
//    if (major)
//      worldTimeZones.add(zoneId)
//  }
//
//  tz("us", "America/New_York", major = true)
//  tz("us", "America/Chicago", major = true)
//  tz("us", "America/Denver", major = true)
//  tz("us", "America/Los_Angeles", major = true)
//  tz("us", "America/Anchorage", major = true)
//  tz("us", "America/Halifax", major = true)
//  tz("us", "Pacific/Honolulu", major = true)
//  tz("ca", "Canada/Pacific")
//  tz("ca", "Canada/Mountain")
//  tz("ca", "Canada/Central")
//  tz("ca", "Canada/Eastern")
//  tz("ca", "Canada/Atlantic")
//  tz("au", "Australia/Sydney", major = true)
//  tz("au", "Australia/Darwin", major = true)
//  tz("au", "Australia/Perth", major = true)
//  tz("ru", "Europe/Moscow", major = true)
//  tz("ru", "Europe/Samara")
//  tz("ru", "Asia/Yekaterinburg")
//  tz("ru", "Asia/Novosibirsk")
//  tz("ru", "Asia/Krasnoyarsk")
//  tz("ru", "Asia/Irkutsk")
//  tz("ru", "Asia/Chita")
//  tz("ru", "Asia/Vladivostok")
//  tz("an", "Europe/Andorra")
//  tz("ae", "Asia/Abu_Dhabi")
//  tz("af", "Asia/Kabul")
//  tz("al", "Europe/Tirana")
//  tz("am", "Asia/Yerevan")
//  tz("ao", "Africa/Luanda")
//  tz("ar", "America/Buenos_Aires")
//  tz("as", "Pacific/Samoa")
//  tz("at", "Europe/Vienna")
//  tz("aw", "America/Aruba")
//  tz("az", "Asia/Baku")
//  tz("ba", "Europe/Sarajevo")
//  tz("bb", "America/Barbados")
//  tz("bd", "Asia/Dhaka")
//  tz("be", "Europe/Brussels", major = true)
//  tz("bf", "Africa/Ouagadougou")
//  tz("bg", "Europe/Sofia")
//  tz("bh", "Asia/Bahrain")
//  tz("bi", "Africa/Bujumbura")
//  tz("bm", "Atlantic/Bermuda")
//  tz("bn", "Asia/Brunei")
//  tz("bo", "America/La_Paz")
//  tz("br", "America/Sao_Paulo")
//  tz("bs", "America/Nassau")
//  tz("bw", "Gaborone")
//  tz("by", "Europe/Minsk")
//  tz("bz", "America/Belize")
//  tz("cd", "Africa/Kinshasa")
//  tz("ch", "Europe/Zurich")
//  tz("ci", "Africa/Abidjan")
//  tz("cl", "America/Santiago")
//  tz("cn", "Asia/Shanghai")
//  tz("co", "America/Bogota")
//  tz("cr", "America/Costa_Rica")
//  tz("cu", "America/Cuba")
//  tz("cv", "Atlantic/Cape_Verde")
//  tz("cy", "Asia/Nicosia")
//  tz("cz", "Europe/Prague")
//  tz("de", "Europe/Berlin")
//  tz("dj", "Africa/Djibouti")
//  tz("dk", "Europe/Copenhagen")
//  tz("do", "America/Santo_Domingo")
//  tz("dz", "Africa/Algiers")
//  tz("ec", "America/Quito")
//  tz("ee", "Europe/Tallinn")
//  tz("eg", "Africa/Cairo")
//  tz("er", "Africa/Asmara")
//  tz("es", "Europe/Madrid")
//  tz("fi", "Europe/Helsinki")
//  tz("fj", "Pacific/Fiji")
//  tz("fk", "America/Stanley")
//  tz("fr", "Europe/Paris")
//  tz("ga", "Africa/Libreville")
//  tz("gb", "Europe/London")
//  tz("gd", "America/Grenada")
//  tz("ge", "Asia/Tbilisi")
//  tz("gh", "Africa/Accra")
//  tz("gm", "Africa/Banjul")
//  tz("gn", "Africa/Conakry")
//  tz("gr", "Europe/Athens")
//  tz("gy", "America/Guyana")
//  tz("hk", "Asia/Hong_Kong")
//  tz("hn", "America/Tegucigalpa")
//  tz("hr", "Europe/Zagreb")
//  tz("ht", "America/Port-au-Prince")
//  tz("hu", "Europe/Budapest")
//  tz("id", "Asia/Jakarta")
//  tz("ie", "Europe/Dublin")
//  tz("il", "Asia/Tel_Aviv", major = true)
//  tz("in", "Asia/Calcutta", major = true)
//  tz("iq", "Asia/Baghdad")
//  tz("ir", "Asia/Tehran")
//  tz("is", "Atlantic/Reykjavik")
//  tz("it", "Europe/Rome")
//  tz("jm", "America/Jamaica")
//  tz("jo", "Asia/Amman")
//  tz("jp", "Asia/Tokyo", major = true)
//  tz("ke", "Africa/Nairobi")
//  tz("kg", "Asia/Bishkek")
//  tz("kh", "Asia/Phnom_Penh")
//  tz("kp", "Asia/Pyongyang")
//  tz("kr", "Asia/Seoul")
//  tz("kw", "Asia/Kuwait")
//  tz("lb", "Asia/Beirut")
//  tz("li", "Europe/Liechtenstein")
//  tz("lk", "Asia/Colombo")
//  tz("lr", "Africa/Monrovia")
//  tz("ls", "Africa/Maseru")
//  tz("lt", "Europe/Vilnius")
//  tz("lu", "Europe/Luxembourg")
//  tz("lv", "Europe/Riga")
//  tz("ly", "Africa/Tripoli")
//  tz("ma", "Africa/Rabat")
//  tz("mc", "Europe/Monaco")
//  tz("md", "Europe/Chisinau")
//  tz("mg", "Indian/Antananarivo")
//  tz("mk", "Europe/Skopje")
//  tz("ml", "Africa/Bamako")
//  tz("mm", "Asia/Rangoon")
//  tz("mn", "Asia/Ulaanbaatar")
//  tz("mo", "Asia/Macao")
//  tz("mq", "America/Martinique")
//  tz("mt", "Europe/Malta")
//  tz("mu", "Indian/Mauritius")
//  tz("mv", "Indian/Maldives")
//  tz("mw", "Africa/Lilongwe")
//  tz("mx", "America/Mexico_City")
//  tz("my", "Asia/Kuala_Lumpur")
//  tz("na", "Africa/Windhoek")
//  tz("ne", "Africa/Niamey")
//  tz("ng", "Africa/Lagos")
//  tz("ni", "America/Managua")
//  tz("nl", "Europe/Amsterdam")
//  tz("no", "Europe/Oslo")
//  tz("np", "Asia/Kathmandu")
//  tz("nz", "Pacific/Aukland")
//  tz("om", "Asia/Muscat")
//  tz("pa", "America/Panama")
//  tz("pe", "America/Lima")
//  tz("pg", "Pacific/Port_Moresby")
//  tz("ph", "Asia/Manila")
//  tz("pk", "Asia/Karachi")
//  tz("pl", "Europe/Warsaw")
//  tz("pr", "America/Puerto_Rico")
//  tz("pt", "Europe/Lisbon")
//  tz("py", "America/Asuncion")
//  tz("qa", "Asia/Qatar")
//  tz("ro", "Europe/Bucharest")
//  tz("rs", "Europe/Belgrade")
//  tz("rw", "Africa/Kigali")
//  tz("sa", "Asia/Riyadh")
//  tz("sd", "Africa/Khartoum")
//  tz("se", "Europe/Stockholm")
//  tz("sg", "Asia/Singapore")
//  tz("si", "Europe/Ljubljana")
//  tz("sk", "Europe/Bratislava")
//  tz("sl", "Africa/Freetown")
//  tz("so", "Africa/Mogadishu")
//  tz("sr", "America/Paramaribo")
//  tz("sv", "America/El_Salvador")
//  tz("sy", "Asia/Damascus")
//  tz("sz", "Africa/Mbabane")
//  tz("td", "Africa/Ndjamena")
//  tz("tg", "Africa/Lome")
//  tz("th", "Asia/Bangkok")
//  tz("tj", "Asia/Dushanbe")
//  tz("tm", "Asia/Ashgabat")
//  tz("tn", "Africa/Tunis")
//  tz("to", "Pacific/Tongatapu")
//  tz("tr", "Asia/Istanbul")
//  tz("tw", "Asia/Taipei")
//  tz("tz", "Africa/Dar_es_Salaam")
//  tz("ua", "Europe/Kiev")
//  tz("ug", "Africa/Kampala")
//  tz("uk", "Europe/London", major = true)
//  tz("uy", "America/Montevideo")
//  tz("uz", "Asia/Tashkent")
//  tz("ve", "America/Caracas")
//  tz("vn", "Asia/Hanoi")
//  tz("za", "Africa/Johannesburg")
//  tz("zm", "Africa/Lusaka")
//  tz("zw", "Africa/Harare")
//
//  def getTimeZoneNameForDate(date: DateTimeValue, place: String): String = {
//
//    if (! date.hasTimezone)
//      return ""
//
//    if (place == null)
//      return formatTimeZoneOffset(date)
//
//    var zone: TimeZone = null
//    var possibleZones: ju.List[String] = null
//    val tzMinutes: Int = date.getTimezoneInMinutes
//    if (place.contains("/")) {
//      zone = getNamedTimeZone(place)
//    } else {
//      possibleZones = idForCountry.get(place.toLowerCase())
//      if (possibleZones == null) {
//        possibleZones = new ju.ArrayList()
//      }
//      // ORBEON: GregorianCalendar
////      val epochDate = date.getCalendar.getTime.getTime
//      val epochDate = date.secondsSinceEpoch.longValue * 1000
//      breakable {
//        for (z <- possibleZones.asScala) {
//          val tz = TimeZone.getTimeZone(z)
//          if (tz != null && tz.getOffset(epochDate) == tzMinutes * 60000) {
//            zone = tz
//            break()
//          }
//        }
//      }
//      if (zone == null) {
//        breakable {
//          for (z <- worldTimeZones.asScala) {
//            val tz: TimeZone = TimeZone.getTimeZone(z)
//            if (tz != null && tz.getOffset(epochDate) == tzMinutes * 60000) {
//              zone = tz
//              break()
//            }
//          }
//        }
//      }
//      if (zone == null)
//        return formatTimeZoneOffset(date)
//    }
//
//
//    val javaDate =
//      try {
//        date.getCalendar.getTime
//      } catch {
//        case _: IllegalArgumentException =>
//          return formatTimeZoneOffset(date)
//
//      }
//    val inSummerTime = zone != null && zone.inDaylightTime(javaDate)
//    zone.getDisplayName(inSummerTime, TimeZone.SHORT)
//  }
//
//  def formatTimeZoneOffset(timeValue: DateTimeValue): String = {
//    val sb = new FastStringBuffer(FastStringBuffer.C16)
//    CalendarValue.appendTimezone(timeValue.getTimezoneInMinutes, sb)
//    sb.toString
//  }
//
//  def getOlsenTimeZoneName(date: DateTimeValue, country: String): String = {
//
//    if (! date.hasTimezone)
//      return ""
//
//    val possibleIds: ju.List[String] = idForCountry.get(country.toLowerCase())
//    var exampleId: String = null
//    if (possibleIds == null) {
//      formatTimeZoneOffset(date)
//    } else {
//      exampleId = possibleIds.get(0)
//    }
//    val exampleZone = TimeZone.getTimeZone(exampleId)
//    val javaDate = date.getCalendar.getTime
//    val inSummerTime: Boolean = exampleZone.inDaylightTime(javaDate)
//    val tzMinutes: Int = date.getTimezoneInMinutes
//    for (i <- 0 until possibleIds.size) {
//      val olsen: String = possibleIds.get(i)
//      val possibleTimeZone: TimeZone = TimeZone.getTimeZone(olsen)
//      val offset: Int = possibleTimeZone.getOffset(javaDate.getTime)
//      if (offset == tzMinutes * 60000) {
//        if (inSummerTime) olsen + "*" else olsen
//      }
//    }
//    formatTimeZoneOffset(date)
//  }
//
//  def inSummerTime(date: DateTimeValue, region: String): java.lang.Boolean = {
//    var olsenName: String = null
//    if (region.length == 2) {
//      val possibleIds: ju.List[String] = idForCountry.get(region.toLowerCase())
//      if (possibleIds == null) {
//        return null
//      } else {
//        olsenName = possibleIds.get(0)
//      }
//    } else {
//      olsenName = region
//    }
//    val zone: TimeZone = TimeZone.getTimeZone(olsenName)
//    java.lang.Boolean.valueOf(zone.inDaylightTime(date.getCalendar.getTime))
//  }
//
//  def civilTimeOffset(date: DateTimeValue, olsenName: String): Int = {
//    val zone = TimeZone.getTimeZone(olsenName)
//    // ORBEON: GregorianCalendar
////    zone.getOffset(date.getCalendar.getTime.getTime)
//    zone.getOffset(date.secondsSinceEpoch.longValue * 1000)
//  }
//
//  def getNamedTimeZone(olsonName: String): TimeZone =
//    if (knownTimeZones.contains(olsonName)) {
//      TimeZone.getTimeZone(olsonName)
//    } else {
//      null
//    }
//
//}
