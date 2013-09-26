package com.rarnu.tools.root.utils;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.Enumeration;

/**
 * @hide
 */
class RemoteAdbd {

    private static final String GET_PORT = "getprop service.adb.tcp.port";
    private static final String SET_PORT = "setprop service.adb.tcp.port 5555";
    private static final String SET_PORT_USB = "setprop service.adb.tcp.port -1";
    private static final String STOP_ADBD = "stop adbd";
    private static final String START_ADBD = "start adbd";

    public static boolean isRemoteConnected() {
        boolean rc = false;
        CommandResult ret = RootUtils.runCommand(GET_PORT, true, null);
        if (ret.error.equals("")) {
            String port = ret.result;
            if (port.equals("5555")) {
                rc = true;
            }
        }
        return rc;
    }

    public static boolean switchAdbd(boolean isStop) {
        boolean r = true;
        CommandResult ret = RootUtils.runCommand((isStop ? SET_PORT_USB : SET_PORT), true, null);
        if (!ret.error.equals("")) {
            r = false;
        }
        if (r) {
            ret = RootUtils.runCommand(STOP_ADBD, true, null);
            if (!ret.error.equals("")) {
                r = false;
            }
        }
        if (r) {
            ret = RootUtils.runCommand(START_ADBD, true, null);
            if (!ret.error.equals("")) {
                r = false;
            }
        }
        return r;
    }

    public static String getIpAddress() {
        try {
            for (Enumeration<NetworkInterface> en = NetworkInterface.getNetworkInterfaces(); en.hasMoreElements(); ) {
                NetworkInterface intf = en.nextElement();
                for (Enumeration<InetAddress> enumIpAddr = intf.getInetAddresses(); enumIpAddr.hasMoreElements(); ) {
                    InetAddress inetAddress = enumIpAddr.nextElement();
                    if (!inetAddress.isLoopbackAddress()) {
                        return inetAddress.getHostAddress().toString();
                    }
                }
            }
        } catch (SocketException ex) {

        }
        return "";
    }

    public static boolean isWifiActive(Context context) {
        Context con = context.getApplicationContext();
        ConnectivityManager connectivity = (ConnectivityManager) con.getSystemService(Context.CONNECTIVITY_SERVICE);
        boolean ret = false;
        if (connectivity != null) {
            NetworkInfo[] info = connectivity.getAllNetworkInfo();
            if (info != null) {
                for (int i = 0; i < info.length; i++) {
                    String typ = info[i].getTypeName().toUpperCase();
                    if (typ.equals("WIFI") && info[i].isConnected()) {
                        ret = true;
                    }
                }
            }
        }
        return ret;
    }
}
