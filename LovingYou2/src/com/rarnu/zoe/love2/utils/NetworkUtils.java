package com.rarnu.zoe.love2.utils;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;

public class NetworkUtils {

        private static ConnectivityManager cmgr = null;

        private static void initConnectManager(Context context) {
                if (cmgr == null) {
                        cmgr = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
                }
        }

        public static NetworkInfo getNetworkInfo(Context context) {
                initConnectManager(context);
                return cmgr.getActiveNetworkInfo();
        }
}