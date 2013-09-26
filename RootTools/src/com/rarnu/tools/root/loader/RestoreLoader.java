package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.utils.ApkUtils;

public class RestoreLoader extends BaseLoader<DataappInfo> {

	public RestoreLoader(Context context) {
		super(context);
	}

	@Override
	public List<DataappInfo> loadInBackground() {
		return ApkUtils.getBackupedApps(getContext(), GlobalInstance.backupPath);
	}

}
