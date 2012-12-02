package com.rarnu.zoe.love2.alarm;

import java.util.Calendar;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.rarnu.zoe.love2.common.Config;
import com.rarnu.zoe.love2.utils.AlarmUtils;
import com.rarnu.zoe.love2.utils.MiscUtils;

public class BootReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {

		long timeDef = MiscUtils.loadDefaultCalendar(11).getTimeInMillis();

		long time = 0;
		Calendar c = null;
		if (Config.getHintEnabled(context, 1)) {
			time = Config.getHintTime(context, 1, timeDef);
			if (time != 0) {
				c = MiscUtils.loadTimeMillis(time);
				if (c != null) {
					AlarmUtils
							.startAlarm(context, 1,
									c.get(Calendar.HOUR_OF_DAY),
									c.get(Calendar.MINUTE));
				}
			}

		}

	}
}
