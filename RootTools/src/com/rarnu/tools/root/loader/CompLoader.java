package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.AsyncTaskLoader;
import android.content.Context;
import android.content.pm.PackageInfo;

import com.rarnu.tools.root.GlobalInstance;

public class CompLoader extends AsyncTaskLoader<List<PackageInfo>> {

	public CompLoader(Context context) {
		super(context);
	}

	@Override
	public List<PackageInfo> loadInBackground() {
		return GlobalInstance.pm.getInstalledPackages(0);
	}

	@Override
	protected void onStartLoading() {
		forceLoad();
	}

	@Override
	public void onCanceled(List<PackageInfo> data) {
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
