package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.AsyncTaskLoader;
import android.content.Context;
import android.util.Log;

import com.rarnu.tools.root.common.SysappInfo;
import com.rarnu.tools.root.utils.ApkUtils;

public class SysappLoader extends AsyncTaskLoader<List<SysappInfo>> {

	public SysappLoader(Context context) {
		super(context);
	}

	@Override
	public List<SysappInfo> loadInBackground() {
		Log.e("SysappLoader", "loadInBackground");
		return ApkUtils.getSystemApps(getContext());
	}

	@Override
	protected void onStartLoading() {
		// super.onStartLoading();
		forceLoad();
	}

	@Override
	public void onCanceled(List<SysappInfo> data) {
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
