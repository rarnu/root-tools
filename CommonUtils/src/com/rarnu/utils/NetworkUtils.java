package com.rarnu.utils;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import com.rarnu.command.CommandResult;
import com.rarnu.command.RootUtils;
import com.rarnu.utils.common.GlobalInstance;
import com.rarnu.utils.common.PingInfo;

import java.io.DataInputStream;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

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

    @SuppressWarnings("deprecation")
    public static String ping(String hostname) {
        String pingResult = "timeout";

        try {
            final Process process = Runtime.getRuntime().exec("ping " + hostname);

            DataInputStream stdout = new DataInputStream(process.getInputStream());
            String line;
            final Timer tmr = new Timer();
            tmr.schedule(new TimerTask() {
                @Override
                public void run() {
                    tmr.cancel();
                    process.destroy();
                }
            }, 3000);

            while ((line = stdout.readLine()) != null) {
                pingResult = line;
                tmr.cancel();
                process.destroy();
                return pingResult;
            }
            process.waitFor();
            return pingResult;
        } catch (Exception e) {
            return pingResult;
        }
    }

    public static String testNetworkSpeed(Context context) {

        CommandResult cmdResult = RootUtils.runCommand("ping -c 5 -s 1024 www.163.com", false, null);
        if (!cmdResult.error.equals("")) {
            return "";
        }

        String[] str = cmdResult.result.split("\n");
        List<PingInfo> list = new ArrayList<PingInfo>();
        PingInfo info = null;
        for (String s : str) {
            info = parseString(s);
            if (info != null) {
                list.add(info);
            }
        }

        return getNetworkSpeed(context, list);
    }

    private static PingInfo parseString(String str) {
        try {
            PingInfo info = null;
            // 1032 bytes from 61.153.56.191: icmp_seq=1 ttl=55 time=11.4 ms
            if (str.contains("icmp_seq=") && str.contains("ttl=") && str.contains("time=")) {
                info = new PingInfo();
                info.byteCount = 1024;
                str = str.replace(" ", "").trim();
                // 1032bytesfrom61.153.56.191:icmp_seq=1ttl=55time=11.4ms
                str = str.substring(str.lastIndexOf("="));
                str = str.replace("ms", "").replace("=", "").trim();
                info.time = Double.parseDouble(str);
            }
            return info;
        } catch (Exception ex) {
            return null;
        }
    }

    private static String getNetworkSpeed(Context context, List<PingInfo> list) {
        if (list == null || list.size() == 0) {
            return "";
        }

        double timeCount = 0D;
        for (PingInfo info : list) {
            timeCount += info.time;
        }
        timeCount /= list.size();
        double speed = 1024 / timeCount;
        String speedStr = new DecimalFormat("#.##").format(speed);
        return String.format("%sK/s", speedStr);
    }

    public static String getNetworkStatusDesc(final Context context) {

        if (GlobalInstance.loadingNetwork) {
            return context.getString(R.string.loading_network_status);
        }

        String status = context.getString(R.string.no_connect_found);
        if (GlobalInstance.networkInfo != null) {

            status = String.format(context.getString(R.string.network_status_fmt),
                    GlobalInstance.networkInfo.getTypeName(),
                    GlobalInstance.networkInfo.getSubtypeName(),
                    networkStatusToReadableString(context, GlobalInstance.networkInfo.getState()),
                    (GlobalInstance.networkInfo.getExtraInfo() == null ? context.getString(R.string.not_contained) : GlobalInstance.networkInfo.getExtraInfo()),
                    (GlobalInstance.networkInfo.isRoaming() ? context.getString(R.string.yes) : context.getString(R.string.no)),
                    (GlobalInstance.networkInfo.isFailover() ? context.getString(R.string.supported) : context.getString(R.string.unsupported)),
                    (GlobalInstance.networkInfo.isAvailable() ? context.getString(R.string.available) : context.getString(R.string.unavailable)),
                    GlobalInstance.networkSpeed
            );
        }

        return status;

    }

    private static String networkStatusToReadableString(Context context, NetworkInfo.State state) {
        switch (state) {
            case CONNECTED:
                return context.getString(R.string.network_connected);
            case CONNECTING:
                return context.getString(R.string.network_connecting);
            case DISCONNECTED:
                return context.getString(R.string.network_disconnected);
            case DISCONNECTING:
                return context.getString(R.string.network_disconnecting);
            case SUSPENDED:
                return context.getString(R.string.network_suspended);
            case UNKNOWN:
                return context.getString(R.string.network_unknown);

        }
        return context.getString(R.string.network_unknown);
    }

    public static void doGetNetworkInfoT(final Context context) {
        new Thread(new Runnable() {

            @Override
            public void run() {
                GlobalInstance.loadingNetwork = true;
                GlobalInstance.networkInfo = NetworkUtils.getNetworkInfo(context);
                GlobalInstance.networkSpeed = NetworkUtils.testNetworkSpeed(context);
                GlobalInstance.loadingNetwork = false;

            }
        }).start();
    }
}
