package com.sbbs.me.android.service;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Handler;
import android.os.Message;
import android.util.Log;

import com.rarnu.utils.DeviceUtilsLite;
import com.rarnu.utils.NotificationUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeUpdate;
import com.sbbs.me.android.consts.Actions;

public class MessageReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(final Context context, final Intent intent) {
		String action = intent.getAction();
		if (action.equals(Actions.ACTION_CHECK_MESSAGE)) {
			// TODO: check message
			Log.e("MessageReceiver", Actions.ACTION_CHECK_MESSAGE);
		} else if (action.equals(Actions.ACTION_CHECK_UPDATE)) {
			// TODO: check update
			Log.e("MessageReceiver", Actions.ACTION_CHECK_UPDATE);
			final Handler hUpdate = new Handler() {
				@Override
				public void handleMessage(Message msg) {
					if (msg.what == 1) {
						SbbsMeUpdate update = (SbbsMeUpdate) msg.obj;
						NotificationUtils.cancelNotication(context,
								Actions.ACTION_NOTIFY_UPDATE);
						NotificationUtils.showNotification(context,
								Actions.ACTION_NOTIFY_UPDATE,
								R.drawable.logo48,
								R.string.notify_update_title,
								R.string.notify_update_desc,
								Actions.ACTION_NOTIFY_UPDATE_CLICK, update,
								true);
					}
					super.handleMessage(msg);
				};
			};

			new Thread(new Runnable() {

				@Override
				public void run() {
					SbbsMeUpdate update = SbbsMeAPI.checkUpdate(DeviceUtilsLite
							.getAppVersionCode(context));
					if (update != null && update.needUpdate) {
						Message msg = new Message();
						msg.what = 1;
						msg.obj = update;
						hUpdate.sendMessage(msg);
					}
				}
			}).start();
		}

	}

}
