package com.zoe.calendar.classes;

import java.io.Serializable;

public class ActivityItem implements Serializable {

	private static final long serialVersionUID = -7427400296609504029L;
	
	public int _id;
	public String city;
	public int year;
	public int startMonth;
	public int startDay;
	public int endMonth;
	public int endDay;
	public int startHour;
	public int startMinute;
	public int endHour;
	public int endMinute;
	public String title;
	public String url;
	public String source;
	public String location;
	public int weight;
	public String tags;
	public String content;
	/**
	 * 0: hide<br>
	 * 1: show
	 */
	public int status;

	private String strStartHour = "";
	private String strStartMinute = "";
	private String strEndHour = "";
	private String strEndMinute = "";

	public String getTimeRegion() {
		String time = "";
		if (startHour != -1) {
			if (strStartHour.equals("")) {
				strStartHour = String.valueOf(startHour);
				if (strStartHour.length() < 2) {
					strStartHour = "0" + strStartHour;
				}
			}
			if (strStartMinute.equals("")) {
				strStartMinute = String.valueOf(startMinute);
				if (strStartMinute.length() < 2) {
					strStartMinute = "0" + strStartMinute;
				}
			}

			time += String.format("%s:%s", strStartHour, strStartMinute);
		}
		if (startHour != -1 && endHour != -1) {
			time += " -- ";
		}
		if (endHour != -1) {
			if (strEndHour.equals("")) {
				strEndHour = String.valueOf(endHour);
				if (strEndHour.length() < 2) {
					strEndHour = "0" + strEndHour;
				}
			}
			if (strEndMinute.equals("")) {
				strEndMinute = String.valueOf(endMinute);
				if (strEndMinute.length() < 2) {
					strEndMinute = "0" + strEndMinute;
				}
			}
			time += String.format("%s:%s", strEndHour, strEndMinute);
		}
		return time;
	}

}
