package com.rarnu.tools.root.receiver;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.utils.NetworkUtils;
import com.rarnu.tools.root.utils.PingUtils;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class NetworkReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(final Context context, Intent intent) {
		if (intent == null) {
			return;
		}
		String action = intent.getAction();
		if (action == null || action.equals("")) {
			return;
		}

		if (action.equals("android.net.conn.CONNECTIVITY_CHANGE")) {
			new Thread(new Runnable() {

				@Override
				public void run() {
					GlobalInstance.loadingNetwork = true;
					GlobalInstance.networkInfo = NetworkUtils.getNetworkInfo(context);
					GlobalInstance.networkSpeed = PingUtils.testNetworkSpeed(context);
					GlobalInstance.loadingNetwork = false;

				}
			}).start();

		}

	}

}
