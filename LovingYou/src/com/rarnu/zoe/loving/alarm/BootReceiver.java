package com.rarnu.zoe.loving.alarm;

import java.util.Calendar;

import com.rarnu.zoe.loving.Config;
import com.rarnu.zoe.loving.utils.AlarmUtils;
import com.rarnu.zoe.loving.utils.MiscUtils;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class BootReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {

		long[] timeDef = new long[] { MiscUtils.loadDefaultCalendar(11)
				.getTimeInMillis(), };

		for (int i = 0; i < 2; i++) {
			long time = 0;
			Calendar c = null;
			if (Config.getHintEnabled(context, i + 1)) {
				time = Config.getHintTime(context, i + 1, timeDef[i]);
				if (time != 0) {
					c = MiscUtils.loadTimeMillis(time);
					if (c != null) {
						AlarmUtils.startAlarm(context, i + 1,
								c.get(Calendar.HOUR_OF_DAY),
								c.get(Calendar.MINUTE));
					}
				}

			}
		}

	}
}
