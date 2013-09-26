package com.rarnu.tools.root.receiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;

public class MutaxReceiver extends BroadcastReceiver {

    public IntentFilter filter = null;
    private OnReceiveMessage onReceive;
    private String actionMain;
    private String actionProgress;
    private String[] actionMutax;

    public MutaxReceiver(String main, String progress, String[] mutax) {
        this.actionMain = main;
        this.actionProgress = progress;
        this.actionMutax = mutax;
        filter = new IntentFilter();
        filter.addAction(main);
        if (progress != null) {
            filter.addAction(progress);
        }
        if (mutax != null && mutax.length != 0) {
            for (String m : mutax) {
                filter.addAction(m);
            }
        }

    }

    public void setOnReceiveMessage(OnReceiveMessage onReceive) {
        this.onReceive = onReceive;
    }

    public void register(Context context) {
        context.registerReceiver(this, filter);
    }

    public void unregister(Context context) {
        context.unregisterReceiver(this);
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent != null) {
            String action = intent.getAction();
            if (action != null) {

                if (action.equals(actionMain)) {
                    boolean operating = intent.getBooleanExtra("operating", false);
                    if (onReceive != null) {
                        onReceive.onStateChange(operating);
                    }
                } else if (action.equals(actionProgress)) {
                    int size = intent.getIntExtra("size", 0);
                    int position = intent.getIntExtra("position", 0);
                    String name = intent.getStringExtra("name");
                    if (onReceive != null) {
                        onReceive.onProgress(name, position, size);
                    }
                } else if (actionInMutax(action)) {
                    boolean operating = intent.getBooleanExtra("operating", false);
                    if (onReceive != null) {
                        onReceive.onMutaxMessage(operating);
                    }
                }
            }
        }
    }

    private boolean actionInMutax(String action) {
        boolean ret = false;
        if (actionMutax != null && actionMutax.length != 0) {
            for (String m : actionMutax) {
                if (m.equals(action)) {
                    ret = true;
                    break;
                }
            }
        }
        return ret;
    }

    public interface OnReceiveMessage {
        void onStateChange(boolean operating);

        void onProgress(String name, int position, int total);

        void onMutaxMessage(boolean operating);
    }

}
