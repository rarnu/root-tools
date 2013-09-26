package com.rarnu.tools.root;

import android.app.Application;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;

public class RootApplication extends Application {

    @Override
    public void onCreate() {
        super.onCreate();
        try {
            ApplicationInfo appInfo = getPackageManager().getApplicationInfo(getPackageName(), PackageManager.GET_META_DATA);

            boolean msg = appInfo.metaData.getBoolean("debug");
            GlobalInstance.DEBUG = msg;
        } catch (Exception e) {
            GlobalInstance.DEBUG = false;
        }

        if (!GlobalInstance.DEBUG) {
            Thread.setDefaultUncaughtExceptionHandler(new RootUncaughtExceptionHandler(this));
        }
    }

    @Override
    public void onTerminate() {
        super.onTerminate();
    }

}
