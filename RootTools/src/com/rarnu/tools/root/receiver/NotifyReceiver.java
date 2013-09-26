package com.rarnu.tools.root.receiver;

import android.content.Context;
import android.content.Intent;
import android.util.Log;
import com.rarnu.devlib.base.BaseNotifyReceiver;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.fragmentactivity.DataappReportActivity;
import com.rarnu.utils.NotificationUtils;

public class NotifyReceiver extends BaseNotifyReceiver {

    @Override
    public void onReceiveNotify(Context context, int id) {
        Log.e("onReceiveNotify", String.valueOf(id));
        NotificationUtils.cancalAllNotification(context, new int[]{
                RTConsts.NOTIFY_ID_HTC_ROM, RTConsts.NOTIFY_ID_BACKUP, RTConsts.NOTIFY_ID_RESTORE, RTConsts.NOTIFY_PROC_BACKUP,
                RTConsts.NOTIFY_PROC_RESTORE, RTConsts.NOTIFY_PROC_HTC_ROM, RTConsts.NOTIFY_ID_CLEAN_BACKUP, RTConsts.NOTIFY_PROC_CLEAN_BACKUP});

        switch (id) {
            case RTConsts.NOTIFY_ID_BACKUP:
            case RTConsts.NOTIFY_ID_RESTORE:
                Intent inReport = new Intent(context, DataappReportActivity.class);
                inReport.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                context.startActivity(inReport);
                break;
        }

    }

}
