package com.rarnu.tools.root;

import java.io.File;

import android.content.Context;
import android.os.Looper;
import android.util.Log;
import android.widget.Toast;

import com.rarnu.tools.root.utils.DirHelper;
import com.rarnu.tools.root.utils.FileUtils;

public class RootUncaughtExceptionHandler implements Thread.UncaughtExceptionHandler {

	private Context context;

	public RootUncaughtExceptionHandler(Context ctx) {
		this.context = ctx;
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
				Toast.makeText(context, R.string.crash_terminate, Toast.LENGTH_LONG).show();
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
		if (GlobalInstance.DEBUG) {
			Log.e("uncaughtException", msg);
			Log.e("sendReport", stack);
			String path = DirHelper.ERROR_DIR + "log.txt";
			File fError = new File(path);
			try {
				if (fError.exists()) {
					FileUtils.appendFile(path, msg + "\n" + stack + "\n\n");
				} else {
					FileUtils.rewriteFile(path, msg + "\n" + stack + "\n\n");
				}
			} catch (Exception ex) {
				Log.e("sendReport", ex.getMessage());
			}
		}

		
	}
}
