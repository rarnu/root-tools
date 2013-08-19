package com.rarnu.battery.notifier.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Observable;

import android.os.BatteryManager;

import com.rarnu.battery.notifier.R;
import com.rarnu.battery.notifier.classes.BatteryState;
import com.rarnu.battery.notifier.utils.BatteryStateStringUtils;
import com.rarnu.utils.ResourceUtils;

public class StateKeeper extends Observable {

	public int status = BatteryManager.BATTERY_STATUS_UNKNOWN;
	public int health = BatteryManager.BATTERY_HEALTH_UNKNOWN;
	public boolean present = false;
	public int level = -1;
	public int scale = -1;
	public int icon_small = -1;
	public int plugged = 0;
	public int voltage = -1;
	public int temperature = -1;
	public String technology = "";
	public int invalid_charger = 0;

	public void setData(int status, int health, boolean present, int level,
			int scale, int icon_small, int plugged, int voltage,
			int temperature, String technology, int invalid_charger) {
		this.status = status;
		this.health = health;
		this.present = present;
		this.level = level;
		this.scale = scale;
		this.icon_small = icon_small;
		this.plugged = plugged;
		this.voltage = voltage;
		this.temperature = temperature;
		this.technology = technology;
		this.invalid_charger = invalid_charger;
		setChanged();
		notifyObservers(this);
	}

	public List<BatteryState> getKeepedList() {
		List<BatteryState> list = new ArrayList<BatteryState>();
		list.add(new BatteryState(ResourceUtils.getString(R.string.b_status),
				BatteryStateStringUtils.statusToString(status)));
		list.add(new BatteryState(ResourceUtils.getString(R.string.b_health),
				BatteryStateStringUtils.healthToString(health)));
		list.add(new BatteryState(ResourceUtils.getString(R.string.b_present),
				BatteryStateStringUtils.presentToString(present)));
		list.add(new BatteryState(ResourceUtils.getString(R.string.b_level),
				String.valueOf(level)));
		list.add(new BatteryState(ResourceUtils.getString(R.string.b_scale),
				String.valueOf(scale)));
		list.add(new BatteryState(ResourceUtils.getString(R.string.b_plugged),
				BatteryStateStringUtils.pluggedToString(plugged)));
		list.add(new BatteryState(ResourceUtils.getString(R.string.b_voltage),
				String.format("%d mV", voltage)));
		list.add(new BatteryState(ResourceUtils
				.getString(R.string.b_temperature), String.format("%.1f C",
				temperature * 1.0F / 10)));
		list.add(new BatteryState(ResourceUtils
				.getString(R.string.b_technology), technology));
		list.add(new BatteryState(ResourceUtils.getString(R.string.b_charger),
				BatteryStateStringUtils.chargerToString(invalid_charger)));
		return list;
	}
}
