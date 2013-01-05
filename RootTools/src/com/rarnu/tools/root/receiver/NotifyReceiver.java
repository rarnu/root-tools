package com.rarnu.tools.root.receiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.utils.NotificationUtils;

public class NotifyReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(final Context context, Intent intent) {
		if (intent == null) {
			return;
		}
		String action = intent.getAction();
		if (action == null || action.equals("")) {
			return;
		}

		if (action.equals(Actions.ACTION_NOTIFY)) {
			NotificationUtils.cancelNotication(context,
					RTConsts.NOTIFY_ID_HTC_ROM);
			NotificationUtils.cancelNotication(context,
					RTConsts.NOTIFY_ID_BACKUP);
			NotificationUtils.cancelNotication(context,
					RTConsts.NOTIFY_ID_RESTORE);

		}

	}

}
