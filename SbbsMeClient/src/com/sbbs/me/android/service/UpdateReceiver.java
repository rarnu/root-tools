package com.sbbs.me.android.service;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.rarnu.utils.NotificationUtils;
import com.sbbs.me.android.api.SbbsMeUpdate;
import com.sbbs.me.android.consts.Actions;
import com.sbbs.me.android.dialog.UpdateInfoDialog;

public class UpdateReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		NotificationUtils.cancelNotication(context,
				Actions.ACTION_NOTIFY_UPDATE);
		if (intent.hasExtra("object")) {
			SbbsMeUpdate update = (SbbsMeUpdate) intent
					.getSerializableExtra("object");
			Intent inUpdate = new Intent(context, UpdateInfoDialog.class);
			inUpdate.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
			inUpdate.putExtra("update", update);
			context.startActivity(inUpdate);
		}
	}

}
