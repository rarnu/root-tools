package com.rarnu.tools.root.utils;

import java.io.DataInputStream;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import android.content.Context;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.PingInfo;
import com.rarnu.tools.root.utils.root.CommandResult;
import com.rarnu.tools.root.utils.root.RootUtils;

public class PingUtils {

	@SuppressWarnings("deprecation")
	public static String ping(String hostname) {
		String pingResult = "timeout";

		try {
			final Process process = Runtime.getRuntime().exec(
					"ping " + hostname);

			DataInputStream stdout = new DataInputStream(
					process.getInputStream());
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

		CommandResult cmdResult = RootUtils.runCommand(
				"ping -c 5 -s 1024 www.163.com", false);
		if (!cmdResult.error.equals("")) {
			return context.getString(R.string.no_speed_tested);
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
			if (str.contains("icmp_seq=") && str.contains("ttl=")
					&& str.contains("time=")) {
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
			return context.getString(R.string.no_speed_tested);
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
}
