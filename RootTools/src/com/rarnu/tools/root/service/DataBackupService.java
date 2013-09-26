package com.rarnu.tools.root.service;

import android.app.Notification;
import android.content.Intent;
import com.rarnu.devlib.base.BaseService;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.common.FragmentNameConst;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.ListUtils;

import java.util.List;

public class DataBackupService extends BaseService {

    private Intent inBackup = new Intent(Actions.ACTION_BACKUP);
    private Intent inBackupProgress = new Intent(Actions.ACTION_BACKUP_PROGRESS);

    @Override
    public void initIntent() {
        inBackup.putExtra("operating", true);
    }

    @Override
    public void fiIntent() {
        inBackup.removeExtra("operating");
        inBackup.putExtra("operating", false);

    }

    @Override
    public Intent getSendIntent() {
        return inBackup;
    }

    @Override
    public void doOperation(String command, Notification n) {
        ApkUtils.clearOperationLog();
        List<DataappInfo> list = ListUtils.getOperateList();

        inBackupProgress.putExtra("size", list.size());
        String backupPath = GlobalInstance.backupPath;
        for (int i = 0; i < list.size(); i++) {

            inBackupProgress.putExtra("position", i + 1);
            inBackupProgress.putExtra("name", GlobalInstance.pm.getApplicationLabel(list.get(i).info).toString());
            sendBroadcast(inBackupProgress);
            ApkUtils.backupData(getApplicationContext(), list.get(i).info.sourceDir, list.get(i).info.packageName, backupPath, list.get(i));

        }

    }

    @Override
    public boolean getCommandCondition(String command) {
        return command.equals(FragmentNameConst.FN_BACKUP);
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
