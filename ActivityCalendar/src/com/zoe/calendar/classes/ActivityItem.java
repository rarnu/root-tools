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

	public static ActivityItem fromRemote(RemoteActivityItem ri) {
		ActivityItem item = new ActivityItem();
		item._id = ri._id;
		item.city = ri.city;
		String rStartDate = ri.startDate;
		String[] starts = rStartDate.split("-");
		item.year = Integer.parseInt(starts[0]);
		item.startMonth = Integer.parseInt(starts[1]);
		item.startDay = Integer.parseInt(starts[2]);
		String rStartTime = ri.startTime;
		if (rStartTime.equals("")) {
			item.startMinute = -1;
			item.startHour = -1;
		} else {
			String[] startTimes = rStartTime.split(":");
			item.startHour = Integer.parseInt(startTimes[0]);
			item.startMinute = Integer.parseInt(startTimes[1]);
		}
		String rEndDate = ri.endDate;
		if (rEndDate.equals("")) {
			item.endMonth = item.startMonth;
			item.endDay = item.startDay;
		} else {
			String[] ends = rEndDate.split("-");
			item.endMonth = Integer.parseInt(ends[1]);
			item.endDay = Integer.parseInt(ends[2]);
		}
		String rEndTime = ri.endTime;
		if (rEndTime.equals("")) {
			item.endHour = -1;
			item.endMinute = -1;
		} else {
			String[] endTimes = rEndTime.split(":");
			item.endHour = Integer.parseInt(endTimes[0]);
			item.endMinute = Integer.parseInt(endTimes[1]);
		}
		item.title = ri.title;
		item.url = ri.url;
		item.source = ri.source;
		if (item.source == null) {
			item.source = "";
		}
		item.location = ri.location;
		if (item.location == null) {
			item.location = "";
		}
		item.weight = ri.weight;
		item.tags = ri.tags;
		if (item.tags == null) {
			item.tags = "";
		}
		item.content = ri.content;
		item.status = 1;
		return item;
		
	}
}
