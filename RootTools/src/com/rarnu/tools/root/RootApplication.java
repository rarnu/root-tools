package com.rarnu.tools.root;

import android.app.Application;

public class RootApplication extends Application {
	@Override
	public void onCreate() {
		super.onCreate();
		if (!GlobalInstance.DEBUG) {
			Thread.setDefaultUncaughtExceptionHandler(new RootUncaughtExceptionHandler(this));
		}
	}
}
