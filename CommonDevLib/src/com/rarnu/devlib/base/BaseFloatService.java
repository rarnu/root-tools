package com.rarnu.devlib.base;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;
import android.view.LayoutInflater;
import android.view.View;
import com.rarnu.devlib.component.FloatWindow;
import com.rarnu.devlib.component.intf.FloatWindowListener;

/**
 * <uses-permission android:name="android.permission.SYSTEM_ALERT_WINDOW" /> <br />
 * must add the service to manifest
 */
public abstract class BaseFloatService extends Service implements FloatWindowListener {

    FloatWindow fv;
    View view;

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onDestroy() {
        fv.hide();
        super.onDestroy();
    }

    public abstract int getViewResId();

    public abstract void initView(View view);

    public abstract int getX();

    public abstract int getY();

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        LayoutInflater inflater = LayoutInflater.from(this);
        view = inflater.inflate(getViewResId(), null);
        initView(view);
        fv = new FloatWindow(this, view, this);
        int init_x = intent.getIntExtra("x", -1);
        int init_y = intent.getIntExtra("y", -1);
        if (init_x == -1) {
            init_x = getX();
        }
        if (init_y == -1) {
            init_y = getY();
        }
        fv.show(init_x, init_y);
        return super.onStartCommand(intent, flags, startId);
    }
}