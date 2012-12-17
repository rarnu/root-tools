package com.rarnu.zoe.love2;

import android.app.Application;

public class RootApplication extends Application {

	@Override
	public void onCreate() {
		super.onCreate();
		Thread.setDefaultUncaughtExceptionHandler(new RootUncaughtExceptionHandler(this));
	}

	@Override
	public void onTerminate() {
		super.onTerminate();
	}
}
