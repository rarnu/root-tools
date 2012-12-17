package com.rarnu.zoe.love2;

import android.content.Context;
import android.os.Looper;

import com.rarnu.zoe.love2.api.LovingYouApi;

public class RootUncaughtExceptionHandler implements
		Thread.UncaughtExceptionHandler {

	private Context context;

	public RootUncaughtExceptionHandler(Context context) {
		this.context = context;
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
				Looper.loop();
			}
		}).start();
		try {
			Thread.sleep(2000);
		} catch (InterruptedException e) {

		}

		System.exit(0);
	}

	private void sendReport(String msg, String stack) {
		LovingYouApi.saveLog(context, "error", msg + "\n" + stack);
	}
}
