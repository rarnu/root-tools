package com.rarnu.battery.notifier.utils;

import com.rarnu.battery.notifier.R;
import com.rarnu.utils.ResourceUtils;

import android.os.BatteryManager;

public class BatteryStateStringUtils {

	public static String statusToString(int status) {
		String str = ResourceUtils.getString(R.string.b_status_unknown);
		switch (status) {
		case BatteryManager.BATTERY_STATUS_UNKNOWN:
			str = ResourceUtils.getString(R.string.b_status_unknown);
			break;
		case BatteryManager.BATTERY_STATUS_CHARGING:
			str = ResourceUtils.getString(R.string.b_status_charging);
			break;
		case BatteryManager.BATTERY_STATUS_DISCHARGING:
			str = ResourceUtils.getString(R.string.b_status_discharging);
			break;
		case BatteryManager.BATTERY_STATUS_NOT_CHARGING:
			str = ResourceUtils.getString(R.string.b_status_not_charging);
			break;
		case BatteryManager.BATTERY_STATUS_FULL:
			str = ResourceUtils.getString(R.string.b_status_full);
			break;
		}
		return str;
	}

	public static String healthToString(int health) {
		String str = ResourceUtils.getString(R.string.b_health_unknown);
		switch (health) {
		case BatteryManager.BATTERY_HEALTH_UNKNOWN:
			str = ResourceUtils.getString(R.string.b_health_unknown);
			break;
		case BatteryManager.BATTERY_HEALTH_COLD:
			str = ResourceUtils.getString(R.string.b_health_cold);
			break;
		case BatteryManager.BATTERY_HEALTH_DEAD:
			str = ResourceUtils.getString(R.string.b_health_dead);
			break;
		case BatteryManager.BATTERY_HEALTH_GOOD:
			str = ResourceUtils.getString(R.string.b_health_good);
			break;
		case BatteryManager.BATTERY_HEALTH_OVER_VOLTAGE:
			str = ResourceUtils.getString(R.string.b_health_over_voltage);
			break;
		case BatteryManager.BATTERY_HEALTH_OVERHEAT:
			str = ResourceUtils.getString(R.string.b_health_overheat);
			break;
		case BatteryManager.BATTERY_HEALTH_UNSPECIFIED_FAILURE:
			str = ResourceUtils
					.getString(R.string.b_health_unspecified_failure);
			break;
		}
		return str;
	}

	public static String presentToString(boolean present) {
		return ResourceUtils.getString(present ? R.string.b_present_true
				: R.string.b_present_false);
	}

	public static String pluggedToString(int plugged) {
		String str = ResourceUtils.getString(R.string.b_plugged_battery);
		switch (plugged) {
		case BatteryManager.BATTERY_PLUGGED_AC:
			str = ResourceUtils.getString(R.string.b_plugged_ac);
			break;
		case BatteryManager.BATTERY_PLUGGED_USB:
			str = ResourceUtils.getString(R.string.b_plugged_usb);
			break;
		}
		return str;
	}

	public static String chargerToString(int charger) {
		return ResourceUtils.getString(charger == 0 ? R.string.b_charger_valid
				: R.string.b_charger_invalid);
	}
}
