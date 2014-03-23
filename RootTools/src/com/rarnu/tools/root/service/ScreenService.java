package com.rarnu.tools.root.service;

import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.PixelFormat;
import android.os.Handler;
import android.os.IBinder;
import android.view.View;
import android.view.WindowManager;
import com.rarnu.tools.root.component.ColorLayer;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class ScreenService extends Service {

    public static int r, g, b;
    public static int rv, gv, bv;
    public static boolean state = false;
    static Handler handler = new Handler();
    private static ColorLayer viewR;
    private static ColorLayer viewG;
    private static ColorLayer viewB;
    ScheduledExecutorService service;

    public static void setRGB(int ir, int ig, int ib, int vr, int vg, int vb) {
        r = ir;
        g = ig;
        b = ib;
        rv = vr;
        gv = vg;
        bv = vb;
        if ((viewR == null) || (viewG == null) || (viewB == null)) {
            return;
        }
        if (r == 0 && rv == 255) {
            viewR.setVisibility(View.GONE);
        } else {
            viewR.setColor(r, rv, 0, 0);
            viewR.setVisibility(View.VISIBLE);
        }
        if (g == 0 && gv == 255) {
            viewG.setVisibility(View.GONE);
        } else {
            viewG.setColor(g, 0, gv, 0);
            viewG.setVisibility(View.VISIBLE);
        }
        if (b == 0 && bv == 255) {
            viewB.setVisibility(View.GONE);
        } else {
            viewB.setColor(b, 0, 0, bv);
            viewB.setVisibility(View.VISIBLE);
        }

    }

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        state = true;
        SharedPreferences pref = getSharedPreferences("pref", 0);
        r = pref.getInt("r", 0);
        g = pref.getInt("g", 0);
        b = pref.getInt("b", 0);
        rv = pref.getInt("rv", 255);
        gv = pref.getInt("gv", 255);
        bv = pref.getInt("bv", 255);
        viewR = new ColorLayer(this);
        viewG = new ColorLayer(this);
        viewB = new ColorLayer(this);

        setRGB(r, g, b, rv, gv, bv);
        WindowManager.LayoutParams lp = new WindowManager.LayoutParams(
                WindowManager.LayoutParams.MATCH_PARENT,
                WindowManager.LayoutParams.MATCH_PARENT,
                WindowManager.LayoutParams.TYPE_SYSTEM_OVERLAY,
                WindowManager.LayoutParams.FLAG_NOT_TOUCHABLE | WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE | WindowManager.LayoutParams.FLAG_NOT_TOUCH_MODAL,
                PixelFormat.TRANSPARENT);
        WindowManager wm = (WindowManager) getSystemService(Context.WINDOW_SERVICE);
        wm.addView(viewB, lp);
        wm.addView(viewG, lp);
        wm.addView(viewR, lp);
    }

    @Override
    public void onDestroy() {
        state = false;
        if (viewR != null) {
            WindowManager wm = (WindowManager) getSystemService(Context.WINDOW_SERVICE);
            wm.removeView(viewR);
            wm.removeView(viewG);
            wm.removeView(viewB);
            viewR = null;
            viewG = null;
            viewB = null;
        }
        super.onDestroy();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        if (this.service == null) {
            this.service = Executors.newSingleThreadScheduledExecutor();
            this.service.scheduleAtFixedRate(new Task(), 0L, 4L, TimeUnit.MILLISECONDS);
        }
        return START_STICKY;
    }

    public void stop() {
        handler.post(new Runnable() {
            public void run() {
                ScreenService.this.service.shutdown();
            }
        });
    }

    class Task implements Runnable {

        public void run() {
            ScreenService.this.stop();
        }
    }
}
