package com.sbbs.me.android.service;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.rarnu.utils.NotificationUtils;
import com.sbbs.me.android.PrivateMessageActivity;
import com.sbbs.me.android.consts.Actions;

public class MessageClickReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		NotificationUtils.cancelNotication(context,
				Actions.ACTION_NOTIFY_MESSAGE);
		Intent inMessage = new Intent(context, PrivateMessageActivity.class);
		inMessage.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP
				| Intent.FLAG_ACTIVITY_NEW_TASK);
		context.startActivity(inMessage);
	}

}
