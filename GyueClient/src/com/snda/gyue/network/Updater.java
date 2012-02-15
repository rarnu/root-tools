package com.snda.gyue.network;

import org.apache.http.protocol.HTTP;

import android.content.Context;
import android.os.Handler;
import android.os.Message;

import com.snda.gyue.utils.MiscUtils;

public class Updater {

	private static int versionCode = 1;
	private static final String updateUrl = "http://rarnu.7thgen.info/snda/gyue/update.php";
	public static final String updateApk = "http://rarnu.7thgen.info/snda/gyue/gyue.apk";
	
	public static void checkUpdate(final Context context, final Handler hUpdate) {
		
		if (MiscUtils.getNetworkType(context) == 0) {
			return;
		}
		
		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					if (msg.arg1 == 1) {
						hUpdate.sendEmptyMessage(99);
					}
				}
				super.handleMessage(msg);
			}
		};
		
		new Thread(new Runnable() {
			
			@Override
			public void run() {
				// 
				int update = 0;
				try {
					String ret = HttpProxy.CallGet(updateUrl, "", HTTP.UTF_8);
					int ver = Integer.parseInt(ret);
					if (ver > versionCode) {
						update = 1;
					}
				} catch (Exception e) {
					
				}
				
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = update;
				h.sendMessage(msg);
				
			}
		}).start();
	}
	
}
