package com.rarnu.tools.root.service;

import android.app.Notification;
import android.content.Intent;
import com.rarnu.devlib.base.BaseService;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.common.FragmentNameConst;
import com.rarnu.tools.root.utils.BackupRestoreUtils;
import com.rarnu.tools.root.utils.ListUtils;

import java.util.List;

public class BatchUninstallService extends BaseService {

    private Intent inUninstall = new Intent(Actions.ACTION_BATCH_UNINSTALL);
    private Intent inUninstallProgress = new Intent(Actions.ACTION_BATCH_UNINSTALL_PROGRESS);

    @Override
    public void initIntent() {
        inUninstall.putExtra("operating", true);
    }

    @Override
    public void fiIntent() {
        inUninstall.removeExtra("operating");
        inUninstall.putExtra("operating", false);

    }

    @Override
    public Intent getSendIntent() {
        return inUninstall;
    }

    @Override
    public void doOperation(String command, Notification n) {
        BackupRestoreUtils.clearOperationLog();
        List<DataappInfo> list = ListUtils.getOperateList();

        inUninstallProgress.putExtra("size", list.size());
        for (int i = 0; i < list.size(); i++) {

            inUninstallProgress.putExtra("position", i + 1);
            inUninstallProgress.putExtra("name", getPackageManager().getApplicationLabel(list.get(i).info).toString());
            sendBroadcast(inUninstallProgress);
            // uninstall application
            BackupRestoreUtils.uninstallApp(getApplicationContext(), list.get(i).info.packageName, list.get(i));
        }

    }

    @Override
    public boolean getCommandCondition(String command) {
        return command.equals(FragmentNameConst.FN_BATCH_UNINSTALL);
    }

    @Override
    public boolean showNotification() {
        return true;
    }

    @Override
    public int getIcon24() {
        return R.drawable.icon24;
    }

}
