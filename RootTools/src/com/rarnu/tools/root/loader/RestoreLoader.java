package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.AsyncTaskLoader;
import android.content.Context;

import com.rarnu.tools.root.common.DataappInfo;
import com.rarnu.tools.root.utils.ApkUtils;

public class RestoreLoader extends AsyncTaskLoader<List<DataappInfo>> {

	public RestoreLoader(Context context) {
		super(context);
	}

	@Override
	public List<DataappInfo> loadInBackground() {
		return ApkUtils.getBackupedApps(getContext());
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
