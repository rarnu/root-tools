package com.rarnu.battery.notifier.service;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.BatteryManager;

import com.rarnu.battery.notifier.Global;
import com.rarnu.battery.notifier.common.Config;
import com.rarnu.battery.notifier.utils.CommandUtils;

public class BatteryReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		String action = intent.getAction();
		if (action != null) {
			if (action.equals(Intent.ACTION_BATTERY_CHANGED)) {
				int status = intent.getIntExtra(BatteryManager.EXTRA_STATUS,
						BatteryManager.BATTERY_STATUS_UNKNOWN);
				int health = intent.getIntExtra(BatteryManager.EXTRA_HEALTH,
						BatteryManager.BATTERY_HEALTH_UNKNOWN);
				boolean present = intent.getBooleanExtra(
						BatteryManager.EXTRA_PRESENT, false);
				int level = intent.getIntExtra(BatteryManager.EXTRA_LEVEL, -1);
				int scale = intent.getIntExtra(BatteryManager.EXTRA_SCALE, -1);
				int icon_small = intent.getIntExtra(
						BatteryManager.EXTRA_ICON_SMALL, -1);
				int plugged = intent.getIntExtra(BatteryManager.EXTRA_PLUGGED,
						0);
				int voltage = intent.getIntExtra(BatteryManager.EXTRA_VOLTAGE,
						-1);
				int temperature = intent.getIntExtra(
						BatteryManager.EXTRA_TEMPERATURE, -1);
				String technology = intent
						.getStringExtra(BatteryManager.EXTRA_TECHNOLOGY);
				int invalid_charger = intent.getIntExtra(
						BatteryManager.EXTRA_INVALID_CHARGER, 0);
				Global.keeper.setData(status, health, present, level, scale,
						icon_small, plugged, voltage, temperature, technology,
						invalid_charger);

				if (level < Config.getCanCharge(context)) {
					CommandUtils.sendStartChargeCommand();
				} else if (level < Config.getFastCharge(context)) {
					CommandUtils.sendFastChargeCommand();
				} else if (status == BatteryManager.BATTERY_STATUS_FULL) {
					CommandUtils.sendStopChargeCommand();
				}
			}
		}
	}

	
}
