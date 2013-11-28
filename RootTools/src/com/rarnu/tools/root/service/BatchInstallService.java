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

public class BatchInstallService extends BaseService {

    private Intent inInstall = new Intent(Actions.ACTION_BATCH_INSTALL);
    private Intent inInstallProgress = new Intent(Actions.ACTION_BATCH_INSTALL_PROGRESS);

    @Override
    public void initIntent() {
        inInstall.putExtra("operating", true);
    }

    @Override
    public void fiIntent() {
        inInstall.removeExtra("operating");
        inInstall.putExtra("operating", false);

    }

    @Override
    public Intent getSendIntent() {
        return inInstall;
    }

    @Override
    public void doOperation(String command, Notification n) {
        BackupRestoreUtils.clearOperationLog();
        List<DataappInfo> list = ListUtils.getOperateList();

        inInstallProgress.putExtra("size", list.size());
        String installPath = GlobalInstance.batchInstallPath;
        for (int i = 0; i < list.size(); i++) {

            inInstallProgress.putExtra("position", i + 1);
            inInstallProgress.putExtra("name", GlobalInstance.pm.getApplicationLabel(list.get(i).info).toString());
            sendBroadcast(inInstallProgress);
            // install application
            BackupRestoreUtils.installApp(getApplicationContext(), list.get(i).localPath, list.get(i));
        }

    }

    @Override
    public boolean getCommandCondition(String command) {
        return command.equals(FragmentNameConst.FN_BATCH_INSTALL);
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
