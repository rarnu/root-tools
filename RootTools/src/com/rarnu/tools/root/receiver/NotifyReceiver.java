package com.rarnu.tools.root.receiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.rarnu.devlib.utils.NotificationUtils;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.fragmentactivity.DataappReportActivity;

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
			int id = intent.getIntExtra("id", 0);
			switch (id) {
			case RTConsts.NOTIFY_ID_BACKUP:
			case RTConsts.NOTIFY_ID_RESTORE:
				Intent inReport = new Intent(context,
						DataappReportActivity.class);
				inReport.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
				context.startActivity(inReport);
				break;
			}

			NotificationUtils.cancalAllNotification(context, new int[] {
					RTConsts.NOTIFY_ID_HTC_ROM, RTConsts.NOTIFY_ID_BACKUP,
					RTConsts.NOTIFY_ID_RESTORE, RTConsts.NOTIFY_PROC_BACKUP,
					RTConsts.NOTIFY_PROC_RESTORE, RTConsts.NOTIFY_PROC_HTC_ROM,
					RTConsts.NOTIFY_ID_CLEAN_BACKUP,
					RTConsts.NOTIFY_PROC_CLEAN_BACKUP });

		}

	}

}
