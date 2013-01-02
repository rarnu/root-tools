package com.rarnu.tools.root.receiver;

import java.io.File;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Handler;
import android.os.Message;

import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.utils.ApkUtils;

public class InstallReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(final Context context, final Intent intent) {
		final String apkPath = intent.getStringExtra("path");
		File apk = new File(apkPath);
		if (!apk.exists()) {
			return;
		}
		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					Intent inInstallResult = new Intent(Actions.ACTION_INSALL_RESULT);
					// msg.arg1 = (installOK ? 1 : 0);
					inInstallResult.putExtra("result", msg.arg1);
					context.sendBroadcast(inInstallResult);
				}
				super.handleMessage(msg);
			}
		};
		new Thread(new Runnable() {
			
			@Override
			public void run() {
				ApkUtils.installSystemApp(context, apkPath, h);
			}
		}).start();
		
	}

}
