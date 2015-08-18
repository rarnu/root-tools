package com.rarnu.devlib.component.zxing;

import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import com.rarnu.devlib.component.zxing.listener.FinishListener;

import java.util.concurrent.*;

public final class InactivityTimer {

    private static final int INACTIVITY_DELAY_SECONDS = 5 * 60;

    private final ScheduledExecutorService inactivityTimer =
            Executors.newSingleThreadScheduledExecutor(new DaemonThreadFactory());
    private final Activity activity;
    private ScheduledFuture<?> inactivityFuture = null;
    private final BroadcastReceiver powerStatusReceiver = new PowerStatusReceiver();

    public InactivityTimer(Activity activity) {
        this.activity = activity;
        onActivity();
    }

    public void onActivity() {
        cancel();
        if (!inactivityTimer.isShutdown()) {
            try {
                inactivityFuture = inactivityTimer.schedule(new FinishListener(activity),
                        INACTIVITY_DELAY_SECONDS,
                        TimeUnit.SECONDS);
            } catch (RejectedExecutionException ree) {

            }
        }
    }

    public void onPause() {
        cancel();
        activity.unregisterReceiver(powerStatusReceiver);
    }

    public void onResume() {
        activity.registerReceiver(powerStatusReceiver, new IntentFilter(Intent.ACTION_BATTERY_CHANGED));
        onActivity();
    }

    private void cancel() {
        ScheduledFuture<?> future = inactivityFuture;
        if (future != null) {
            future.cancel(true);
            inactivityFuture = null;
        }
    }

    public void shutdown() {
        cancel();
        inactivityTimer.shutdown();
    }

    private static final class DaemonThreadFactory implements ThreadFactory {
        @Override
        public Thread newThread(Runnable runnable) {
            Thread thread = new Thread(runnable);
            thread.setDaemon(true);
            return thread;
        }
    }

    private final class PowerStatusReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            if (Intent.ACTION_BATTERY_CHANGED.equals(intent.getAction())) {
                int batteryPlugged = intent.getIntExtra("plugged", -1);
                if (batteryPlugged > 0) {
                    InactivityTimer.this.cancel();
                }
            }
        }
    }

}
