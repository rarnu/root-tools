package com.rarnu.devlib.component;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import com.rarnu.utils.NetworkUtils;

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
            NetworkUtils.doGetNetworkInfoT(context);

        }

    }

}
