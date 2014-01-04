package com.rarnu.tools.root.service;

import android.app.AlarmManager;
import android.app.Service;
import android.content.Intent;
import android.os.IBinder;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.utils.AlarmUtils;

public class WidgetKeepService extends Service {

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        flags = START_REDELIVER_INTENT;
        AlarmUtils.startAlarm(getApplicationContext(), AlarmManager.RTC, 0, 60 * 5 * 1000, Actions.ACTION_WIDGET_KEEP);
        return super.onStartCommand(intent, flags, startId);
    }

    @Override
    public void onDestroy() {
        AlarmUtils.cancelAlarm(getApplicationContext(), 0, Actions.ACTION_WIDGET_KEEP);
        super.onDestroy();
    }


}
