package com.rarnu.battery.notifier.common;

import com.rarnu.utils.ConfigUtils;

import android.content.Context;

public class Config {

	private static final String KEY_BATTERY_CAN_CHARGE = "key_battery_can_charge";
	private static final String KEY_BATTERY_FAST_CHARGE = "key_battery_fast_charge";
	
	public static void setCanCharge(Context context, int value) {
		ConfigUtils.setIntConfig(context, KEY_BATTERY_CAN_CHARGE, value);
	}
	
	public static int getCanCharge(Context context) {
		return ConfigUtils.getIntConfig(context, KEY_BATTERY_CAN_CHARGE, 80);
	}
	
	public static void setFastCharge(Context context, int value) {
		ConfigUtils.setIntConfig(context, KEY_BATTERY_FAST_CHARGE, value);
	}
	
	public static int getFastCharge(Context context) {
		return ConfigUtils.getIntConfig(context, KEY_BATTERY_FAST_CHARGE, 15);
	}

}
