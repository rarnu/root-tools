package com.rarnu.battery.notifier.classes;

public class BatteryState {

	public String itemName;
	public String stateName;

	public BatteryState(String name, String state) {
		this.itemName = name;
		this.stateName = state;
	}
}
