package com.sbbs.me.android;

import android.app.Application;

import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeLogs;
import com.sbbs.me.android.utils.ExceptionThread;

public class SbbsMeApplication extends Application {

	@Override
	public void onCreate() {
		super.onCreate();
		SbbsMeAPI.writeLogT(this, SbbsMeLogs.LOG_START, "");
		Thread.setDefaultUncaughtExceptionHandler(new ExceptionThread(this));
	}

	@Override
	public void onTerminate() {
		SbbsMeAPI.writeLogT(this, SbbsMeLogs.LOG_END, "");
		super.onTerminate();
	}

}
