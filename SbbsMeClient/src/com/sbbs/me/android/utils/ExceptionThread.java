package com.sbbs.me.android.utils;

import java.lang.Thread.UncaughtExceptionHandler;

import android.content.Context;
import android.os.Looper;
import android.widget.Toast;

import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeLogs;

public class ExceptionThread implements UncaughtExceptionHandler {

	private Context mContext;

	public ExceptionThread(Context context) {
		this.mContext = context;
	}

	@Override
	public void uncaughtException(Thread thread, Throwable ex) {
		
		final String msg = ex.getMessage();
		String err = "";
		StackTraceElement[] ste = ex.getStackTrace();
		for (StackTraceElement e : ste) {
			err += e.toString() + "\n";
		}
		final String transErr = err;

		new Thread(new Runnable() {

			@Override
			public void run() {
				sendReport(msg, transErr);
				Looper.prepare();
				Toast.makeText(mContext, R.string.crash_terminate,
						Toast.LENGTH_LONG).show();
				Looper.loop();
			}
		}).start();
		try {
			Thread.sleep(3000);
		} catch (InterruptedException e) {

		}

		System.exit(0);

	}

	private void sendReport(String msg, String stack) {
		SbbsMeAPI.writeLogT(mContext, SbbsMeLogs.LOG_CRASH, "message:" + msg
				+ "\nstack:" + stack);
	}

}
