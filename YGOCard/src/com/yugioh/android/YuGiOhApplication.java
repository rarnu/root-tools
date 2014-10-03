package com.yugioh.android;

import android.app.Application;
import android.content.Intent;
import com.yugioh.android.define.PathDefine;

public class YuGiOhApplication extends Application {

    @Override
    public void onCreate() {
        super.onCreate();
        PathDefine.init();
    }

    @Override
    public void onTerminate() {
        super.onTerminate();
    }
}
