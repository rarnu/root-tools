package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.AsyncTaskLoader;
import android.content.Context;

import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.utils.ApkUtils;

public class BackupLoader extends AsyncTaskLoader<List<DataappInfo>> {

	public BackupLoader(Context context) {
		super(context);
	}

	@Override
	public List<DataappInfo> loadInBackground() {
		return ApkUtils.getInstalledApps(getContext(), false);
	}

	@Override
	protected void onStartLoading() {
		forceLoad();
	}

	@Override
	public void onCanceled(List<DataappInfo> data) {
		super.onCanceled(data);
	}

	@Override
	protected void onStopLoading() {
		cancelLoad();
	}

	@Override
	protected void onReset() {
		stopLoading();

	}
}
