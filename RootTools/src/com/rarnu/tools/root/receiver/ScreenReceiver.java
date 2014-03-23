package com.rarnu.tools.root.receiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import com.rarnu.tools.root.service.ScreenService;

public class ScreenReceiver extends BroadcastReceiver {
    @Override
    public void onReceive(final Context context, final Intent intent) {
        Log.e("ScreenReceiver", "Intent.ACTION_BOOT_COMPLETED");
        String action = intent.getAction();
        if (action.equals(Intent.ACTION_BOOT_COMPLETED)) {
            context.startService(new Intent(context, ScreenService.class));
        }
    }
}
