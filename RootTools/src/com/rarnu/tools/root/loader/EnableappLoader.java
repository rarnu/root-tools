package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.AsyncTaskLoader;
import android.content.Context;

import com.rarnu.tools.root.common.EnableappInfo;
import com.rarnu.tools.root.utils.ApkUtils;

public class EnableappLoader extends AsyncTaskLoader<List<EnableappInfo>> {

	public EnableappLoader(Context context) {
		super(context);
	}

	@Override
	public List<EnableappInfo> loadInBackground() {
		return ApkUtils.getEnabledApplications(getContext());
	}

	@Override
	protected void onStartLoading() {
		forceLoad();
	}

	@Override
	public void onCanceled(List<EnableappInfo> data) {
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
