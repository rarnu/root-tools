package com.rarnu.tools.root.base;

import android.app.Service;
import android.content.Intent;
import android.os.Handler;
import android.os.IBinder;
import android.os.Message;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.NotificationUtils;

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

	public abstract void doOperation(String command);
	
	public abstract boolean getCommandCondition(String command);

	private void operation(final String command, final int id, final int title, final int desc) {

		ApkUtils.clearOperationLog();

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {

				if (msg.what == 1) {
					operating = false;
					fiIntent();
					sendBroadcast(getSendIntent());
					doNotification(id, title, desc);
				}
				super.handleMessage(msg);
			};
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				doOperation(command);
				h.sendEmptyMessage(1);

			}
		}).start();

	}

	private void doNotification(int id, int title, int desc) {
		NotificationUtils.showNotification(getApplicationContext(), id,
				R.drawable.icon24, title, desc, Actions.ACTION_NOTIFY);
	}

	@Override
	public int onStartCommand(Intent intent, int flags, int startId) {
		if (intent != null) {
			String command = intent.getStringExtra("command");
			int id = intent.getIntExtra("id", 0);
			int title = intent.getIntExtra("title", 0);
			int desc = intent.getIntExtra("desc", 0);
			if (command != null) {
				if (getCommandCondition(command)) {
					operating = true;
					initIntent();
					doSendMessage();
					operation(command, id, title, desc);
				}
			}
		}
		return super.onStartCommand(intent, flags, startId);
	}

}
