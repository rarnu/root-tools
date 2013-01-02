package com.rarnu.tools.root.receiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.os.Handler;
import android.os.Message;

import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.utils.ApkUtils;

public class UninstallReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(final Context context, final Intent intent) {
		boolean backup = intent.getBooleanExtra("backup", true);
		boolean deleteData = intent.getBooleanExtra("deleteData", true);
		ApplicationInfo info = (ApplicationInfo) intent
				.getParcelableExtra("info");
		deleteApp(context, info, backup, deleteData);
	}

	public void deleteApp(final Context context, final ApplicationInfo info,
			final boolean backup, final boolean deleteData) {
		// need delete app's data also

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					Intent inRet = new Intent(Actions.ACTION_UNINSTALL_RESULT);
					inRet.putExtra("result", msg.arg1);
					context.sendBroadcast(inRet);
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				if (backup) {
					ApkUtils.backupSystemApp(info.sourceDir);
				}

				LogApi.logDeleteSystemApp(info.packageName);

				boolean ret = ApkUtils.deleteSystemApp(info.sourceDir);
				if (ret) {
					if (deleteData) {
						ApkUtils.deleteSystemAppData(info.dataDir);
					}
				}
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = ret ? 1 : 0;
				h.sendMessage(msg);

			}
		}).start();

	}

}
