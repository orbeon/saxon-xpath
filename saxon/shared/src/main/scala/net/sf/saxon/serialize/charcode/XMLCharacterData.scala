////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
package net.sf.saxon.serialize.charcode

import net.sf.saxon.z.IntRangeSet

import java.util.Arrays




object XMLCharacterData {

  private val data: Array[Byte] = new Array[Byte](65536)

  /**
    * Bit setting to indicate that a character is valid in XML 1.0
    */ /**
    * Bit setting to indicate that a character is valid in XML 1.0
    */
  val VALID_10_MASK: Byte = 1

  /**
    * Bit setting to indicate that a character is valid in an XML 1.0 name
    */ /**
    * Bit setting to indicate that a character is valid in an XML 1.0 name
    */
  val NAME_10_MASK: Byte = 2

  /**
    * Bit setting to indicate that a character is valid at the start of an XML 1.0 name
    */ /**
    * Bit setting to indicate that a character is valid at the start of an XML 1.0 name
    */
  val NAME_START_10_MASK: Byte = 4

  /**
    * Bit setting to indicate that a character is valid in XML 1.1
    */ /**
    * Bit setting to indicate that a character is valid in XML 1.1
    */
  val VALID_11_MASK: Byte = 8

  /**
    * Bit setting to indicate that a character is valid in an XML 1.1 name
    */ /**
    * Bit setting to indicate that a character is valid in an XML 1.1 name
    */
  val NAME_11_MASK: Byte = 16

  /**
    * Bit setting to indicate that a character is valid at the start of an XML 1.1 name
    */ /**
    * Bit setting to indicate that a character is valid at the start of an XML 1.1 name
    */
  val NAME_START_11_MASK: Byte = 32

  /**
    * Maximum code point for a character permitted in an XML 1.1 name
    */ /**
    * Maximum code point for a character permitted in an XML 1.1 name
    */
  val MAX_XML11_NAME_CHAR: Int = 0xEFFFF

  def isValid10(i: Int): Boolean =
    if (i < 65536) (data(i) & VALID_10_MASK) != 0
    else
      (UTF16CharacterSet.NONBMP_MIN <= i && i <= UTF16CharacterSet.NONBMP_MAX)

  def isNCName10(i: Int): Boolean = i < 65536 && (data(i) & NAME_10_MASK) != 0

  def isNCNameStart10(i: Int): Boolean =
    i < 65536 && (data(i) & NAME_START_10_MASK) != 0

  def isValid11(i: Int): Boolean =
    if (i < 65536) (data(i) & VALID_11_MASK) != 0
    else
      (UTF16CharacterSet.NONBMP_MIN <= i && i <= UTF16CharacterSet.NONBMP_MAX)

  def isNCName11(i: Int): Boolean =
    if (i < 65536) (data(i) & NAME_11_MASK) != 0
    else (UTF16CharacterSet.NONBMP_MIN <= i && i <= MAX_XML11_NAME_CHAR)

  def isNCNameStart11(i: Int): Boolean =
    if (i < 65536) (data(i) & NAME_START_11_MASK) != 0
    else (UTF16CharacterSet.NONBMP_MIN <= i && i <= MAX_XML11_NAME_CHAR)

  data(0) = 0.toByte

  Arrays.fill(data, 1, 9, 8.toByte)

  Arrays.fill(data, 9, 11, 9.toByte)

  Arrays.fill(data, 11, 13, 8.toByte)

  data(13) = 9.toByte

  Arrays.fill(data, 14, 32, 8.toByte)

  Arrays.fill(data, 32, 45, 9.toByte)

  Arrays.fill(data, 45, 47, 27.toByte)

  data(47) = 9.toByte

  Arrays.fill(data, 48, 58, 27.toByte)

// colon
  data(58) = 9.toByte

  Arrays.fill(data, 59, 65, 9.toByte)

  Arrays.fill(data, 65, 91, 63.toByte)

  Arrays.fill(data, 91, 95, 9.toByte)

  data(95) = 63.toByte

  data(96) = 9.toByte

  Arrays.fill(data, 97, 123, 63.toByte)

  Arrays.fill(data, 123, 183, 9.toByte)

  data(183) = 27.toByte

  Arrays.fill(data, 184, 192, 9.toByte)

  Arrays.fill(data, 192, 215, 63.toByte)

  data(215) = 9.toByte

  Arrays.fill(data, 216, 247, 63.toByte)

  data(247) = 9.toByte

  Arrays.fill(data, 248, 306, 63.toByte)

  Arrays.fill(data, 306, 308, 57.toByte)

  Arrays.fill(data, 308, 319, 63.toByte)

  Arrays.fill(data, 319, 321, 57.toByte)

  Arrays.fill(data, 321, 329, 63.toByte)

  data(329) = 57.toByte

  Arrays.fill(data, 330, 383, 63.toByte)

  data(383) = 57.toByte

  Arrays.fill(data, 384, 452, 63.toByte)

  Arrays.fill(data, 452, 461, 57.toByte)

  Arrays.fill(data, 461, 497, 63.toByte)

  Arrays.fill(data, 497, 500, 57.toByte)

  Arrays.fill(data, 500, 502, 63.toByte)

  Arrays.fill(data, 502, 506, 57.toByte)

  Arrays.fill(data, 506, 536, 63.toByte)

  Arrays.fill(data, 536, 592, 57.toByte)

  Arrays.fill(data, 592, 681, 63.toByte)

  Arrays.fill(data, 681, 699, 57.toByte)

  Arrays.fill(data, 699, 706, 63.toByte)

  Arrays.fill(data, 706, 720, 57.toByte)

  Arrays.fill(data, 720, 722, 59.toByte)

  Arrays.fill(data, 722, 768, 57.toByte)

  Arrays.fill(data, 768, 838, 27.toByte)

  Arrays.fill(data, 838, 864, 25.toByte)

  Arrays.fill(data, 864, 866, 27.toByte)

  Arrays.fill(data, 866, 880, 25.toByte)

  Arrays.fill(data, 880, 894, 57.toByte)

  data(894) = 9.toByte

  Arrays.fill(data, 895, 902, 57.toByte)

  data(902) = 63.toByte

  data(903) = 59.toByte

  Arrays.fill(data, 904, 907, 63.toByte)

  data(907) = 57.toByte

  data(908) = 63.toByte

  data(909) = 57.toByte

  Arrays.fill(data, 910, 930, 63.toByte)

  data(930) = 57.toByte

  Arrays.fill(data, 931, 975, 63.toByte)

  data(975) = 57.toByte

  Arrays.fill(data, 976, 983, 63.toByte)

  Arrays.fill(data, 983, 986, 57.toByte)

  data(986) = 63.toByte

  data(987) = 57.toByte

  data(988) = 63.toByte

  data(989) = 57.toByte

  data(990) = 63.toByte

  data(991) = 57.toByte

  data(992) = 63.toByte

  data(993) = 57.toByte

  Arrays.fill(data, 994, 1012, 63.toByte)

  Arrays.fill(data, 1012, 1025, 57.toByte)

  Arrays.fill(data, 1025, 1037, 63.toByte)

  data(1037) = 57.toByte

  Arrays.fill(data, 1038, 1104, 63.toByte)

  data(1104) = 57.toByte

  Arrays.fill(data, 1105, 1117, 63.toByte)

  data(1117) = 57.toByte

  Arrays.fill(data, 1118, 1154, 63.toByte)

  data(1154) = 57.toByte

  Arrays.fill(data, 1155, 1159, 59.toByte)

  Arrays.fill(data, 1159, 1168, 57.toByte)

  Arrays.fill(data, 1168, 1221, 63.toByte)

  Arrays.fill(data, 1221, 1223, 57.toByte)

  Arrays.fill(data, 1223, 1225, 63.toByte)

  Arrays.fill(data, 1225, 1227, 57.toByte)

  Arrays.fill(data, 1227, 1229, 63.toByte)

  Arrays.fill(data, 1229, 1232, 57.toByte)

  Arrays.fill(data, 1232, 1260, 63.toByte)

  Arrays.fill(data, 1260, 1262, 57.toByte)

  Arrays.fill(data, 1262, 1270, 63.toByte)

  Arrays.fill(data, 1270, 1272, 57.toByte)

  Arrays.fill(data, 1272, 1274, 63.toByte)

  Arrays.fill(data, 1274, 1329, 57.toByte)

  Arrays.fill(data, 1329, 1367, 63.toByte)

  Arrays.fill(data, 1367, 1369, 57.toByte)

  data(1369) = 63.toByte

  Arrays.fill(data, 1370, 1377, 57.toByte)

  Arrays.fill(data, 1377, 1415, 63.toByte)

  Arrays.fill(data, 1415, 1425, 57.toByte)

  Arrays.fill(data, 1425, 1442, 59.toByte)

  data(1442) = 57.toByte

  Arrays.fill(data, 1443, 1466, 59.toByte)

  data(1466) = 57.toByte

  Arrays.fill(data, 1467, 1470, 59.toByte)

  data(1470) = 57.toByte

  data(1471) = 59.toByte

  data(1472) = 57.toByte

  Arrays.fill(data, 1473, 1475, 59.toByte)

  data(1475) = 57.toByte

  data(1476) = 59.toByte

  Arrays.fill(data, 1477, 1488, 57.toByte)

  Arrays.fill(data, 1488, 1515, 63.toByte)

  Arrays.fill(data, 1515, 1520, 57.toByte)

  Arrays.fill(data, 1520, 1523, 63.toByte)

  Arrays.fill(data, 1523, 1569, 57.toByte)

  Arrays.fill(data, 1569, 1595, 63.toByte)

  Arrays.fill(data, 1595, 1600, 57.toByte)

  data(1600) = 59.toByte

  Arrays.fill(data, 1601, 1611, 63.toByte)

  Arrays.fill(data, 1611, 1619, 59.toByte)

  Arrays.fill(data, 1619, 1632, 57.toByte)

  Arrays.fill(data, 1632, 1642, 59.toByte)

  Arrays.fill(data, 1642, 1648, 57.toByte)

  data(1648) = 59.toByte

  Arrays.fill(data, 1649, 1720, 63.toByte)

  Arrays.fill(data, 1720, 1722, 57.toByte)

  Arrays.fill(data, 1722, 1727, 63.toByte)

  data(1727) = 57.toByte

  Arrays.fill(data, 1728, 1743, 63.toByte)

  data(1743) = 57.toByte

  Arrays.fill(data, 1744, 1748, 63.toByte)

  data(1748) = 57.toByte

  data(1749) = 63.toByte

  Arrays.fill(data, 1750, 1765, 59.toByte)

  Arrays.fill(data, 1765, 1767, 63.toByte)

  Arrays.fill(data, 1767, 1769, 59.toByte)

  data(1769) = 57.toByte

  Arrays.fill(data, 1770, 1774, 59.toByte)

  Arrays.fill(data, 1774, 1776, 57.toByte)

  Arrays.fill(data, 1776, 1786, 59.toByte)

  Arrays.fill(data, 1786, 2305, 57.toByte)

  Arrays.fill(data, 2305, 2308, 59.toByte)

  data(2308) = 57.toByte

  Arrays.fill(data, 2309, 2362, 63.toByte)

  Arrays.fill(data, 2362, 2364, 57.toByte)

  data(2364) = 59.toByte

  data(2365) = 63.toByte

  Arrays.fill(data, 2366, 2382, 59.toByte)

  Arrays.fill(data, 2382, 2385, 57.toByte)

  Arrays.fill(data, 2385, 2389, 59.toByte)

  Arrays.fill(data, 2389, 2392, 57.toByte)

  Arrays.fill(data, 2392, 2402, 63.toByte)

  Arrays.fill(data, 2402, 2404, 59.toByte)

  Arrays.fill(data, 2404, 2406, 57.toByte)

  Arrays.fill(data, 2406, 2416, 59.toByte)

  Arrays.fill(data, 2416, 2433, 57.toByte)

  Arrays.fill(data, 2433, 2436, 59.toByte)

  data(2436) = 57.toByte

  Arrays.fill(data, 2437, 2445, 63.toByte)

  Arrays.fill(data, 2445, 2447, 57.toByte)

  Arrays.fill(data, 2447, 2449, 63.toByte)

  Arrays.fill(data, 2449, 2451, 57.toByte)

  Arrays.fill(data, 2451, 2473, 63.toByte)

  data(2473) = 57.toByte

  Arrays.fill(data, 2474, 2481, 63.toByte)

  data(2481) = 57.toByte

  data(2482) = 63.toByte

  Arrays.fill(data, 2483, 2486, 57.toByte)

  Arrays.fill(data, 2486, 2490, 63.toByte)

  Arrays.fill(data, 2490, 2492, 57.toByte)

  data(2492) = 59.toByte

  data(2493) = 57.toByte

  Arrays.fill(data, 2494, 2501, 59.toByte)

  Arrays.fill(data, 2501, 2503, 57.toByte)

  Arrays.fill(data, 2503, 2505, 59.toByte)

  Arrays.fill(data, 2505, 2507, 57.toByte)

  Arrays.fill(data, 2507, 2510, 59.toByte)

  Arrays.fill(data, 2510, 2519, 57.toByte)

  data(2519) = 59.toByte

  Arrays.fill(data, 2520, 2524, 57.toByte)

  Arrays.fill(data, 2524, 2526, 63.toByte)

  data(2526) = 57.toByte

  Arrays.fill(data, 2527, 2530, 63.toByte)

  Arrays.fill(data, 2530, 2532, 59.toByte)

  Arrays.fill(data, 2532, 2534, 57.toByte)

  Arrays.fill(data, 2534, 2544, 59.toByte)

  Arrays.fill(data, 2544, 2546, 63.toByte)

  Arrays.fill(data, 2546, 2562, 57.toByte)

  data(2562) = 59.toByte

  Arrays.fill(data, 2563, 2565, 57.toByte)

  Arrays.fill(data, 2565, 2571, 63.toByte)

  Arrays.fill(data, 2571, 2575, 57.toByte)

  Arrays.fill(data, 2575, 2577, 63.toByte)

  Arrays.fill(data, 2577, 2579, 57.toByte)

  Arrays.fill(data, 2579, 2601, 63.toByte)

  data(2601) = 57.toByte

  Arrays.fill(data, 2602, 2609, 63.toByte)

  data(2609) = 57.toByte

  Arrays.fill(data, 2610, 2612, 63.toByte)

  data(2612) = 57.toByte

  Arrays.fill(data, 2613, 2615, 63.toByte)

  data(2615) = 57.toByte

  Arrays.fill(data, 2616, 2618, 63.toByte)

  Arrays.fill(data, 2618, 2620, 57.toByte)

  data(2620) = 59.toByte

  data(2621) = 57.toByte

  Arrays.fill(data, 2622, 2627, 59.toByte)

  Arrays.fill(data, 2627, 2631, 57.toByte)

  Arrays.fill(data, 2631, 2633, 59.toByte)

  Arrays.fill(data, 2633, 2635, 57.toByte)

  Arrays.fill(data, 2635, 2638, 59.toByte)

  Arrays.fill(data, 2638, 2649, 57.toByte)

  Arrays.fill(data, 2649, 2653, 63.toByte)

  data(2653) = 57.toByte

  data(2654) = 63.toByte

  Arrays.fill(data, 2655, 2662, 57.toByte)

  Arrays.fill(data, 2662, 2674, 59.toByte)

  Arrays.fill(data, 2674, 2677, 63.toByte)

  Arrays.fill(data, 2677, 2689, 57.toByte)

  Arrays.fill(data, 2689, 2692, 59.toByte)

  data(2692) = 57.toByte

  Arrays.fill(data, 2693, 2700, 63.toByte)

  data(2700) = 57.toByte

  data(2701) = 63.toByte

  data(2702) = 57.toByte

  Arrays.fill(data, 2703, 2706, 63.toByte)

  data(2706) = 57.toByte

  Arrays.fill(data, 2707, 2729, 63.toByte)

  data(2729) = 57.toByte

  Arrays.fill(data, 2730, 2737, 63.toByte)

  data(2737) = 57.toByte

  Arrays.fill(data, 2738, 2740, 63.toByte)

  data(2740) = 57.toByte

  Arrays.fill(data, 2741, 2746, 63.toByte)

  Arrays.fill(data, 2746, 2748, 57.toByte)

  data(2748) = 59.toByte

  data(2749) = 63.toByte

  Arrays.fill(data, 2750, 2758, 59.toByte)

  data(2758) = 57.toByte

  Arrays.fill(data, 2759, 2762, 59.toByte)

  data(2762) = 57.toByte

  Arrays.fill(data, 2763, 2766, 59.toByte)

  Arrays.fill(data, 2766, 2784, 57.toByte)

  data(2784) = 63.toByte

  Arrays.fill(data, 2785, 2790, 57.toByte)

  Arrays.fill(data, 2790, 2800, 59.toByte)

  Arrays.fill(data, 2800, 2817, 57.toByte)

  Arrays.fill(data, 2817, 2820, 59.toByte)

  data(2820) = 57.toByte

  Arrays.fill(data, 2821, 2829, 63.toByte)

  Arrays.fill(data, 2829, 2831, 57.toByte)

  Arrays.fill(data, 2831, 2833, 63.toByte)

  Arrays.fill(data, 2833, 2835, 57.toByte)

  Arrays.fill(data, 2835, 2857, 63.toByte)

  data(2857) = 57.toByte

  Arrays.fill(data, 2858, 2865, 63.toByte)

  data(2865) = 57.toByte

  Arrays.fill(data, 2866, 2868, 63.toByte)

  Arrays.fill(data, 2868, 2870, 57.toByte)

  Arrays.fill(data, 2870, 2874, 63.toByte)

  Arrays.fill(data, 2874, 2876, 57.toByte)

  data(2876) = 59.toByte

  data(2877) = 63.toByte

  Arrays.fill(data, 2878, 2884, 59.toByte)

  Arrays.fill(data, 2884, 2887, 57.toByte)

  Arrays.fill(data, 2887, 2889, 59.toByte)

  Arrays.fill(data, 2889, 2891, 57.toByte)

  Arrays.fill(data, 2891, 2894, 59.toByte)

  Arrays.fill(data, 2894, 2902, 57.toByte)

  Arrays.fill(data, 2902, 2904, 59.toByte)

  Arrays.fill(data, 2904, 2908, 57.toByte)

  Arrays.fill(data, 2908, 2910, 63.toByte)

  data(2910) = 57.toByte

  Arrays.fill(data, 2911, 2914, 63.toByte)

  Arrays.fill(data, 2914, 2918, 57.toByte)

  Arrays.fill(data, 2918, 2928, 59.toByte)

  Arrays.fill(data, 2928, 2946, 57.toByte)

  Arrays.fill(data, 2946, 2948, 59.toByte)

  data(2948) = 57.toByte

  Arrays.fill(data, 2949, 2955, 63.toByte)

  Arrays.fill(data, 2955, 2958, 57.toByte)

  Arrays.fill(data, 2958, 2961, 63.toByte)

  data(2961) = 57.toByte

  Arrays.fill(data, 2962, 2966, 63.toByte)

  Arrays.fill(data, 2966, 2969, 57.toByte)

  Arrays.fill(data, 2969, 2971, 63.toByte)

  data(2971) = 57.toByte

  data(2972) = 63.toByte

  data(2973) = 57.toByte

  Arrays.fill(data, 2974, 2976, 63.toByte)

  Arrays.fill(data, 2976, 2979, 57.toByte)

  Arrays.fill(data, 2979, 2981, 63.toByte)

  Arrays.fill(data, 2981, 2984, 57.toByte)

  Arrays.fill(data, 2984, 2987, 63.toByte)

  Arrays.fill(data, 2987, 2990, 57.toByte)

  Arrays.fill(data, 2990, 2998, 63.toByte)

  data(2998) = 57.toByte

  Arrays.fill(data, 2999, 3002, 63.toByte)

  Arrays.fill(data, 3002, 3006, 57.toByte)

  Arrays.fill(data, 3006, 3011, 59.toByte)

  Arrays.fill(data, 3011, 3014, 57.toByte)

  Arrays.fill(data, 3014, 3017, 59.toByte)

  data(3017) = 57.toByte

  Arrays.fill(data, 3018, 3022, 59.toByte)

  Arrays.fill(data, 3022, 3031, 57.toByte)

  data(3031) = 59.toByte

  Arrays.fill(data, 3032, 3047, 57.toByte)

  Arrays.fill(data, 3047, 3056, 59.toByte)

  Arrays.fill(data, 3056, 3073, 57.toByte)

  Arrays.fill(data, 3073, 3076, 59.toByte)

  data(3076) = 57.toByte

  Arrays.fill(data, 3077, 3085, 63.toByte)

  data(3085) = 57.toByte

  Arrays.fill(data, 3086, 3089, 63.toByte)

  data(3089) = 57.toByte

  Arrays.fill(data, 3090, 3113, 63.toByte)

  data(3113) = 57.toByte

  Arrays.fill(data, 3114, 3124, 63.toByte)

  data(3124) = 57.toByte

  Arrays.fill(data, 3125, 3130, 63.toByte)

  Arrays.fill(data, 3130, 3134, 57.toByte)

  Arrays.fill(data, 3134, 3141, 59.toByte)

  data(3141) = 57.toByte

  Arrays.fill(data, 3142, 3145, 59.toByte)

  data(3145) = 57.toByte

  Arrays.fill(data, 3146, 3150, 59.toByte)

  Arrays.fill(data, 3150, 3157, 57.toByte)

  Arrays.fill(data, 3157, 3159, 59.toByte)

  Arrays.fill(data, 3159, 3168, 57.toByte)

  Arrays.fill(data, 3168, 3170, 63.toByte)

  Arrays.fill(data, 3170, 3174, 57.toByte)

  Arrays.fill(data, 3174, 3184, 59.toByte)

  Arrays.fill(data, 3184, 3202, 57.toByte)

  Arrays.fill(data, 3202, 3204, 59.toByte)

  data(3204) = 57.toByte

  Arrays.fill(data, 3205, 3213, 63.toByte)

  data(3213) = 57.toByte

  Arrays.fill(data, 3214, 3217, 63.toByte)

  data(3217) = 57.toByte

  Arrays.fill(data, 3218, 3241, 63.toByte)

  data(3241) = 57.toByte

  Arrays.fill(data, 3242, 3252, 63.toByte)

  data(3252) = 57.toByte

  Arrays.fill(data, 3253, 3258, 63.toByte)

  Arrays.fill(data, 3258, 3262, 57.toByte)

  Arrays.fill(data, 3262, 3269, 59.toByte)

  data(3269) = 57.toByte

  Arrays.fill(data, 3270, 3273, 59.toByte)

  data(3273) = 57.toByte

  Arrays.fill(data, 3274, 3278, 59.toByte)

  Arrays.fill(data, 3278, 3285, 57.toByte)

  Arrays.fill(data, 3285, 3287, 59.toByte)

  Arrays.fill(data, 3287, 3294, 57.toByte)

  data(3294) = 63.toByte

  data(3295) = 57.toByte

  Arrays.fill(data, 3296, 3298, 63.toByte)

  Arrays.fill(data, 3298, 3302, 57.toByte)

  Arrays.fill(data, 3302, 3312, 59.toByte)

  Arrays.fill(data, 3312, 3330, 57.toByte)

  Arrays.fill(data, 3330, 3332, 59.toByte)

  data(3332) = 57.toByte

  Arrays.fill(data, 3333, 3341, 63.toByte)

  data(3341) = 57.toByte

  Arrays.fill(data, 3342, 3345, 63.toByte)

  data(3345) = 57.toByte

  Arrays.fill(data, 3346, 3369, 63.toByte)

  data(3369) = 57.toByte

  Arrays.fill(data, 3370, 3386, 63.toByte)

  Arrays.fill(data, 3386, 3390, 57.toByte)

  Arrays.fill(data, 3390, 3396, 59.toByte)

  Arrays.fill(data, 3396, 3398, 57.toByte)

  Arrays.fill(data, 3398, 3401, 59.toByte)

  data(3401) = 57.toByte

  Arrays.fill(data, 3402, 3406, 59.toByte)

  Arrays.fill(data, 3406, 3415, 57.toByte)

  data(3415) = 59.toByte

  Arrays.fill(data, 3416, 3424, 57.toByte)

  Arrays.fill(data, 3424, 3426, 63.toByte)

  Arrays.fill(data, 3426, 3430, 57.toByte)

  Arrays.fill(data, 3430, 3440, 59.toByte)

  Arrays.fill(data, 3440, 3585, 57.toByte)

  Arrays.fill(data, 3585, 3631, 63.toByte)

  data(3631) = 57.toByte

  data(3632) = 63.toByte

  data(3633) = 59.toByte

  Arrays.fill(data, 3634, 3636, 63.toByte)

  Arrays.fill(data, 3636, 3643, 59.toByte)

  Arrays.fill(data, 3643, 3648, 57.toByte)

  Arrays.fill(data, 3648, 3654, 63.toByte)

  Arrays.fill(data, 3654, 3663, 59.toByte)

  data(3663) = 57.toByte

  Arrays.fill(data, 3664, 3674, 59.toByte)

  Arrays.fill(data, 3674, 3713, 57.toByte)

  Arrays.fill(data, 3713, 3715, 63.toByte)

  data(3715) = 57.toByte

  data(3716) = 63.toByte

  Arrays.fill(data, 3717, 3719, 57.toByte)

  Arrays.fill(data, 3719, 3721, 63.toByte)

  data(3721) = 57.toByte

  data(3722) = 63.toByte

  Arrays.fill(data, 3723, 3725, 57.toByte)

  data(3725) = 63.toByte

  Arrays.fill(data, 3726, 3732, 57.toByte)

  Arrays.fill(data, 3732, 3736, 63.toByte)

  data(3736) = 57.toByte

  Arrays.fill(data, 3737, 3744, 63.toByte)

  data(3744) = 57.toByte

  Arrays.fill(data, 3745, 3748, 63.toByte)

  data(3748) = 57.toByte

  data(3749) = 63.toByte

  data(3750) = 57.toByte

  data(3751) = 63.toByte

  Arrays.fill(data, 3752, 3754, 57.toByte)

  Arrays.fill(data, 3754, 3756, 63.toByte)

  data(3756) = 57.toByte

  Arrays.fill(data, 3757, 3759, 63.toByte)

  data(3759) = 57.toByte

  data(3760) = 63.toByte

  data(3761) = 59.toByte

  Arrays.fill(data, 3762, 3764, 63.toByte)

  Arrays.fill(data, 3764, 3770, 59.toByte)

  data(3770) = 57.toByte

  Arrays.fill(data, 3771, 3773, 59.toByte)

  data(3773) = 63.toByte

  Arrays.fill(data, 3774, 3776, 57.toByte)

  Arrays.fill(data, 3776, 3781, 63.toByte)

  data(3781) = 57.toByte

  data(3782) = 59.toByte

  data(3783) = 57.toByte

  Arrays.fill(data, 3784, 3790, 59.toByte)

  Arrays.fill(data, 3790, 3792, 57.toByte)

  Arrays.fill(data, 3792, 3802, 59.toByte)

  Arrays.fill(data, 3802, 3864, 57.toByte)

  Arrays.fill(data, 3864, 3866, 59.toByte)

  Arrays.fill(data, 3866, 3872, 57.toByte)

  Arrays.fill(data, 3872, 3882, 59.toByte)

  Arrays.fill(data, 3882, 3893, 57.toByte)

  data(3893) = 59.toByte

  data(3894) = 57.toByte

  data(3895) = 59.toByte

  data(3896) = 57.toByte

  data(3897) = 59.toByte

  Arrays.fill(data, 3898, 3902, 57.toByte)

  Arrays.fill(data, 3902, 3904, 59.toByte)

  Arrays.fill(data, 3904, 3912, 63.toByte)

  data(3912) = 57.toByte

  Arrays.fill(data, 3913, 3946, 63.toByte)

  Arrays.fill(data, 3946, 3953, 57.toByte)

  Arrays.fill(data, 3953, 3973, 59.toByte)

  data(3973) = 57.toByte

  Arrays.fill(data, 3974, 3980, 59.toByte)

  Arrays.fill(data, 3980, 3984, 57.toByte)

  Arrays.fill(data, 3984, 3990, 59.toByte)

  data(3990) = 57.toByte

  data(3991) = 59.toByte

  data(3992) = 57.toByte

  Arrays.fill(data, 3993, 4014, 59.toByte)

  Arrays.fill(data, 4014, 4017, 57.toByte)

  Arrays.fill(data, 4017, 4024, 59.toByte)

  data(4024) = 57.toByte

  data(4025) = 59.toByte

  Arrays.fill(data, 4026, 4256, 57.toByte)

  Arrays.fill(data, 4256, 4294, 63.toByte)

  Arrays.fill(data, 4294, 4304, 57.toByte)

  Arrays.fill(data, 4304, 4343, 63.toByte)

  Arrays.fill(data, 4343, 4352, 57.toByte)

  data(4352) = 63.toByte

  data(4353) = 57.toByte

  Arrays.fill(data, 4354, 4356, 63.toByte)

  data(4356) = 57.toByte

  Arrays.fill(data, 4357, 4360, 63.toByte)

  data(4360) = 57.toByte

  data(4361) = 63.toByte

  data(4362) = 57.toByte

  Arrays.fill(data, 4363, 4365, 63.toByte)

  data(4365) = 57.toByte

  Arrays.fill(data, 4366, 4371, 63.toByte)

  Arrays.fill(data, 4371, 4412, 57.toByte)

  data(4412) = 63.toByte

  data(4413) = 57.toByte

  data(4414) = 63.toByte

  data(4415) = 57.toByte

  data(4416) = 63.toByte

  Arrays.fill(data, 4417, 4428, 57.toByte)

  data(4428) = 63.toByte

  data(4429) = 57.toByte

  data(4430) = 63.toByte

  data(4431) = 57.toByte

  data(4432) = 63.toByte

  Arrays.fill(data, 4433, 4436, 57.toByte)

  Arrays.fill(data, 4436, 4438, 63.toByte)

  Arrays.fill(data, 4438, 4441, 57.toByte)

  data(4441) = 63.toByte

  Arrays.fill(data, 4442, 4447, 57.toByte)

  Arrays.fill(data, 4447, 4450, 63.toByte)

  data(4450) = 57.toByte

  data(4451) = 63.toByte

  data(4452) = 57.toByte

  data(4453) = 63.toByte

  data(4454) = 57.toByte

  data(4455) = 63.toByte

  data(4456) = 57.toByte

  data(4457) = 63.toByte

  Arrays.fill(data, 4458, 4461, 57.toByte)

  Arrays.fill(data, 4461, 4463, 63.toByte)

  Arrays.fill(data, 4463, 4466, 57.toByte)

  Arrays.fill(data, 4466, 4468, 63.toByte)

  data(4468) = 57.toByte

  data(4469) = 63.toByte

  Arrays.fill(data, 4470, 4510, 57.toByte)

  data(4510) = 63.toByte

  Arrays.fill(data, 4511, 4520, 57.toByte)

  data(4520) = 63.toByte

  Arrays.fill(data, 4521, 4523, 57.toByte)

  data(4523) = 63.toByte

  Arrays.fill(data, 4524, 4526, 57.toByte)

  Arrays.fill(data, 4526, 4528, 63.toByte)

  Arrays.fill(data, 4528, 4535, 57.toByte)

  Arrays.fill(data, 4535, 4537, 63.toByte)

  data(4537) = 57.toByte

  data(4538) = 63.toByte

  data(4539) = 57.toByte

  Arrays.fill(data, 4540, 4547, 63.toByte)

  Arrays.fill(data, 4547, 4587, 57.toByte)

  data(4587) = 63.toByte

  Arrays.fill(data, 4588, 4592, 57.toByte)

  data(4592) = 63.toByte

  Arrays.fill(data, 4593, 4601, 57.toByte)

  data(4601) = 63.toByte

  Arrays.fill(data, 4602, 7680, 57.toByte)

  Arrays.fill(data, 7680, 7836, 63.toByte)

  Arrays.fill(data, 7836, 7840, 57.toByte)

  Arrays.fill(data, 7840, 7930, 63.toByte)

  Arrays.fill(data, 7930, 7936, 57.toByte)

  Arrays.fill(data, 7936, 7958, 63.toByte)

  Arrays.fill(data, 7958, 7960, 57.toByte)

  Arrays.fill(data, 7960, 7966, 63.toByte)

  Arrays.fill(data, 7966, 7968, 57.toByte)

  Arrays.fill(data, 7968, 8006, 63.toByte)

  Arrays.fill(data, 8006, 8008, 57.toByte)

  Arrays.fill(data, 8008, 8014, 63.toByte)

  Arrays.fill(data, 8014, 8016, 57.toByte)

  Arrays.fill(data, 8016, 8024, 63.toByte)

  data(8024) = 57.toByte

  data(8025) = 63.toByte

  data(8026) = 57.toByte

  data(8027) = 63.toByte

  data(8028) = 57.toByte

  data(8029) = 63.toByte

  data(8030) = 57.toByte

  Arrays.fill(data, 8031, 8062, 63.toByte)

  Arrays.fill(data, 8062, 8064, 57.toByte)

  Arrays.fill(data, 8064, 8117, 63.toByte)

  data(8117) = 57.toByte

  Arrays.fill(data, 8118, 8125, 63.toByte)

  data(8125) = 57.toByte

  data(8126) = 63.toByte

  Arrays.fill(data, 8127, 8130, 57.toByte)

  Arrays.fill(data, 8130, 8133, 63.toByte)

  data(8133) = 57.toByte

  Arrays.fill(data, 8134, 8141, 63.toByte)

  Arrays.fill(data, 8141, 8144, 57.toByte)

  Arrays.fill(data, 8144, 8148, 63.toByte)

  Arrays.fill(data, 8148, 8150, 57.toByte)

  Arrays.fill(data, 8150, 8156, 63.toByte)

  Arrays.fill(data, 8156, 8160, 57.toByte)

  Arrays.fill(data, 8160, 8173, 63.toByte)

  Arrays.fill(data, 8173, 8178, 57.toByte)

  Arrays.fill(data, 8178, 8181, 63.toByte)

  data(8181) = 57.toByte

  Arrays.fill(data, 8182, 8189, 63.toByte)

  Arrays.fill(data, 8189, 8192, 57.toByte)

  Arrays.fill(data, 8192, 8204, 9.toByte)

  Arrays.fill(data, 8204, 8206, 57.toByte)

  Arrays.fill(data, 8206, 8255, 9.toByte)

  Arrays.fill(data, 8255, 8257, 25.toByte)

  Arrays.fill(data, 8257, 8304, 9.toByte)

  Arrays.fill(data, 8304, 8400, 57.toByte)

  Arrays.fill(data, 8400, 8413, 59.toByte)

  Arrays.fill(data, 8413, 8417, 57.toByte)

  data(8417) = 59.toByte

  Arrays.fill(data, 8418, 8486, 57.toByte)

  data(8486) = 63.toByte

  Arrays.fill(data, 8487, 8490, 57.toByte)

  Arrays.fill(data, 8490, 8492, 63.toByte)

  Arrays.fill(data, 8492, 8494, 57.toByte)

  data(8494) = 63.toByte

  Arrays.fill(data, 8495, 8576, 57.toByte)

  Arrays.fill(data, 8576, 8579, 63.toByte)

  Arrays.fill(data, 8579, 8592, 57.toByte)

  Arrays.fill(data, 8592, 11264, 9.toByte)

  Arrays.fill(data, 11264, 12272, 57.toByte)

  Arrays.fill(data, 12272, 12289, 9.toByte)

  Arrays.fill(data, 12289, 12293, 57.toByte)

  data(12293) = 59.toByte

  data(12294) = 57.toByte

  data(12295) = 63.toByte

  Arrays.fill(data, 12296, 12321, 57.toByte)

  Arrays.fill(data, 12321, 12330, 63.toByte)

  Arrays.fill(data, 12330, 12336, 59.toByte)

  data(12336) = 57.toByte

  Arrays.fill(data, 12337, 12342, 59.toByte)

  Arrays.fill(data, 12342, 12353, 57.toByte)

  Arrays.fill(data, 12353, 12437, 63.toByte)

  Arrays.fill(data, 12437, 12441, 57.toByte)

  Arrays.fill(data, 12441, 12443, 59.toByte)

  Arrays.fill(data, 12443, 12445, 57.toByte)

  Arrays.fill(data, 12445, 12447, 59.toByte)

  Arrays.fill(data, 12447, 12449, 57.toByte)

  Arrays.fill(data, 12449, 12539, 63.toByte)

  data(12539) = 57.toByte

  Arrays.fill(data, 12540, 12543, 59.toByte)

  Arrays.fill(data, 12543, 12549, 57.toByte)

  Arrays.fill(data, 12549, 12589, 63.toByte)

  Arrays.fill(data, 12589, 19968, 57.toByte)

  Arrays.fill(data, 19968, 40870, 63.toByte)

  Arrays.fill(data, 40870, 44032, 57.toByte)

  Arrays.fill(data, 44032, 55204, 63.toByte)

  Arrays.fill(data, 55204, 55296, 57.toByte)

  Arrays.fill(data, 55296, 57344, 0.toByte)

  Arrays.fill(data, 57344, 63744, 9.toByte)

  Arrays.fill(data, 63744, 64976, 57.toByte)

  Arrays.fill(data, 64976, 65008, 9.toByte)

  Arrays.fill(data, 65008, 65534, 57.toByte)

  Arrays.fill(data, 65534, 65536, 0.toByte)

  /*@NotNull*/

  def getCategory(mask: Byte): IntRangeSet = {
    val irs: IntRangeSet = new IntRangeSet()
    for (i <- 0.until(65536) if (data(i) & mask) != 0) {
      irs.add(i)
    }
    if ((mask & (NAME_START_11_MASK | NAME_11_MASK)) != 0) {
      irs.addRange(UTF16CharacterSet.NONBMP_MIN, MAX_XML11_NAME_CHAR)
    }
    irs
  }

}

// Copyright (c) 2018-2020 Saxonica Limited
// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.
// If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
// This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla Public License, v. 2.0.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
