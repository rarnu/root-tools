package com.snda.root.remoteadb;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.Enumeration;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;

public class RemoteAdbd {

	public static boolean isRemoteConnected() {
		boolean rc = false;
		CommandResult ret = RootUtils.runRootCommand(Commands.GET_PORT);
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
		CommandResult ret = RootUtils
				.runRootCommand(isStop ? Commands.SET_PORT_USB
						: Commands.SET_PORT);
		if (!ret.error.equals("")) {
			r = false;
		}
		if (r) {
			ret = RootUtils.runRootCommand(Commands.STOP_ADBD);
			if (!ret.error.equals("")) {
				r = false;
			}
		}
		if (r) {
			ret = RootUtils.runRootCommand(Commands.START_ADBD);
			if (!ret.error.equals("")) {
				r = false;
			}
		}
		return r;
	}

	public static String getIpAddress() {
		try {
			for (Enumeration<NetworkInterface> en = NetworkInterface
					.getNetworkInterfaces(); en.hasMoreElements();) {
				NetworkInterface intf = en.nextElement();
				for (Enumeration<InetAddress> enumIpAddr = intf
						.getInetAddresses(); enumIpAddr.hasMoreElements();) {
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
		ConnectivityManager connectivity = (ConnectivityManager) con
				.getSystemService(Context.CONNECTIVITY_SERVICE);
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
