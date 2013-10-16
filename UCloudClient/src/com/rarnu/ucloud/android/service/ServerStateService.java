package com.rarnu.ucloud.android.service;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

public class ServerStateService extends Service {

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {

        return super.onStartCommand(intent, flags, startId);
    }

    @Override
    public void onDestroy() {

        super.onDestroy();
    }
}
