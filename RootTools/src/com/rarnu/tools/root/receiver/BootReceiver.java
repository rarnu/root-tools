package com.rarnu.tools.root.receiver;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.common.RTConfig;
import com.rarnu.tools.root.service.FloatWidgetService;
import com.rarnu.tools.root.utils.IptablesUtils;
import com.rarnu.tools.root.utils.MemorySpecialList;
import com.rarnu.utils.FloatUtils;

public class BootReceiver extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        GlobalInstance.init(context);
        String action = intent.getAction();
        if (action.equals(Intent.ACTION_BOOT_COMPLETED)) {

            MemorySpecialList.loadExcludeList(context);
            RTConfig.initConfig(context);
            showFloatWindow(context);

            if (IptablesUtils.isEnabled(context)) {
                if (!IptablesUtils.applySavedIptablesRules(context)) {
                    IptablesUtils.setEnabled(context, false);
                }
            }
        }
    }

    private void showFloatWindow(Context context) {
        if (GlobalInstance.showFloatWindow) {
            FloatUtils.showFloatWindow(context, FloatWidgetService.class);
        }
    }
}
