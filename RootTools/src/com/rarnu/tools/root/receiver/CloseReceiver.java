package com.rarnu.tools.root.receiver;

import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class CloseReceiver extends BroadcastReceiver {

	private Activity activity;
	public CloseReceiver(Activity activity) {
		super();
		this.activity = activity;
	}
	
	@Override
	public void onReceive(Context context, Intent intent) {
		activity.finish();
	}

}
