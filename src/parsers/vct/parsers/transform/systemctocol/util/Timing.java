package vct.parsers.transform.systemctocol.util;

/**
 * Helper class to keep track of time resolution in the transformed COL system.
 */
public class Timing {

	/**
	 * Time Resolution in nanoseconds. Clock values are calculated from time in nanoseconds.
	 */
	public static long SC_TIME_RESOLUTION = 1000000L;
	
	
	/**
	 * Returns the factor of the given time unit String relative to the current time resolution.
	 *
	 * @param str SystemC-specific time unit descriptor (SC_XX)
	 * @return Conversion factor for the time unit with respect to the time resolution
	 */
	public static double getTimeFactor(String str) {
		return switch (str) {
			case "SC_FS" -> 1.0 / SC_TIME_RESOLUTION;
			case "SC_PS" -> 1000.0 / SC_TIME_RESOLUTION;
			case "SC_NS" -> 1000000.0 / SC_TIME_RESOLUTION;
			case "SC_US" -> 1000000000.0 / SC_TIME_RESOLUTION;
			case "SC_MS" -> 1000000000000.0 / SC_TIME_RESOLUTION;
			case "SC_SEC" -> 1000000000000000.0 / SC_TIME_RESOLUTION;
			default -> throw new IllegalArgumentException("Time factor " + str + " is unknown.");
		};
	}
	
	
	/**
	 * Creates an integer representation of the sc_time arguments given as parameters.
	 * <p>
	 * The problem here is that SystemC uses different units and double values to represent time, which gives
	 * it a much greater range than the integer clocks of Uppaal. A useful solution for that
	 * has yet to be found. This is just a dummy representation, which returns the value in nanoseconds (SC_NS). 
	 * pico and femto seconds are not supported, they will be rounded to one ns.
	 * 
	 * @param time A string containing a number of time units
	 * @param unit A String of the SystemC's sc_time_unit enumeration
	 * @return Total time specified by unit and amount, given in terms of the time resolution
	 *
	 */
	public static long getTimeUnits(String time,  String unit) {
		long units = Long.parseLong(time);
		if (units == 0) return 0;
		
		double factor = getTimeFactor(unit);
		double val = units * factor;
		
		if (val > Long.MAX_VALUE) return Long.MAX_VALUE;

		return Math.round(val);
	}
}
