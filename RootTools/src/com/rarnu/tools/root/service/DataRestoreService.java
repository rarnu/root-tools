package com.rarnu.tools.root.service;

import java.util.List;

import android.app.Notification;
import android.content.Intent;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.base.BaseService;
import com.rarnu.tools.root.common.Actions;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.ListUtils;
import com.rarnu.tools.root.utils.root.RootUtils;

public class DataRestoreService extends BaseService {

	private Intent inRestore = new Intent(Actions.ACTION_RESTORE);
	private Intent inRestoreProgress = new Intent(
			Actions.ACTION_RESTORE_PROGRESS);

	@Override
	public void initIntent() {
		inRestore.putExtra("operating", true);
	}

	@Override
	public void fiIntent() {
		inRestore.removeExtra("operating");
		inRestore.putExtra("operating", false);
	}

	@Override
	public Intent getSendIntent() {
		return inRestore;
	}

	@Override
	public void doOperation(String command, Notification n) {
		RootUtils.runCommand("pm set-install-location 1", true);
		List<DataappInfo> list = ListUtils.getOperateList();
		inRestoreProgress.putExtra("size", list.size());
		for (int i = 0; i < list.size(); i++) {

			inRestoreProgress.putExtra("position", i + 1);
			inRestoreProgress.putExtra("name", GlobalInstance.pm
					.getApplicationLabel(list.get(i).info).toString());
			sendBroadcast(inRestoreProgress);

			ApkUtils.restoreData(
					getApplicationContext(),
					ApkUtils.getLabelFromPackage(getApplicationContext(),
							list.get(i).info), list.get(i).info.packageName,
					list.get(i));

		}

	}

	@Override
	public boolean getCommandCondition(String command) {
		return command.equals("restore");
	}

}
