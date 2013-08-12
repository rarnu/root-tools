package com.sbbs.me.android.service;

import com.sbbs.me.android.consts.Actions;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

public class MessageReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		String action = intent.getAction();
		if (action.equals(Actions.ACTION_CHECK_MESSAGE)) {
			// TODO: check message
			Log.e("MessageReceiver", Actions.ACTION_CHECK_MESSAGE);
		} else if (action.equals(Actions.ACTION_CHECK_UPDATE)) {
			// TODO: check update
			Log.e("MessageReceiver", Actions.ACTION_CHECK_UPDATE);
		}

	}

}
