package com.rarnu.tools.root.base;

import android.app.Notification;
import android.app.Service;
import android.content.Intent;
import android.os.Handler;
import android.os.IBinder;
import android.os.Message;
import android.util.Log;

import com.rarnu.devlib.utils.NotificationUtils;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.utils.ApkUtils;

public abstract class BaseService extends Service {

	private boolean operating = false;

	@Override
	public IBinder onBind(Intent intent) {
		return null;
	}

	private void doSendMessage() {
		new Thread(new Runnable() {

			@Override
			public void run() {
				while (true) {
					if (!operating) {
						break;
					}
					sendBroadcast(getSendIntent());
					try {
						Thread.sleep(500);
					} catch (Exception e) {

					}
				}

			}
		}).start();
	}

	public abstract void initIntent();

	public abstract void fiIntent();

	public abstract Intent getSendIntent();

	public abstract void doOperation(String command, Notification n);

	public abstract boolean getCommandCondition(String command);

	private void operation(final String command, final int id, final int title,
			final int desc, final int proc_id, final int proc_title,
			final int proc_desc) {

		final Notification n = NotificationUtils.buildNotification(
				getApplicationContext(), id, R.drawable.icon24, proc_title,
				proc_desc, Actions.ACTION_NOTIFY_NULL, false);
		startForeground(proc_id, n);

		ApkUtils.clearOperationLog();

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {

				if (msg.what == 1) {
					operating = false;
					fiIntent();
					sendBroadcast(getSendIntent());
					stopForeground(true);
					doNotification(id, title, desc, true, Actions.ACTION_NOTIFY);
				}
				super.handleMessage(msg);
			};
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				doOperation(command, n);
				h.sendEmptyMessage(1);

			}
		}).start();

	}

	private void doNotification(int id, int title, int desc, boolean canClose,
			String action) {

		NotificationUtils.showNotification(getApplicationContext(), id,
				R.drawable.icon24, title, desc, action, canClose);
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId) {
		if (intent != null) {
			String command = intent.getStringExtra("command");
			int id = intent.getIntExtra("id", 0);
			int title = intent.getIntExtra("title", 0);
			int desc = intent.getIntExtra("desc", 0);
			int proc_id = intent.getIntExtra("proc_id", 0);
			int proc_title = intent.getIntExtra("proc_title", 0);
			int proc_desc = intent.getIntExtra("proc_desc", 0);
			if (command != null) {
				if (getCommandCondition(command)) {
					operating = true;
					initIntent();
					doSendMessage();
					operation(command, id, title, desc, proc_id, proc_title,
							proc_desc);
				}
			}
		}
		return super.onStartCommand(intent, flags, startId);
	}

	@Override
	public void onDestroy() {
		Log.e(getClass().getName(), "onDestroy");
		super.onDestroy();
	}

}
