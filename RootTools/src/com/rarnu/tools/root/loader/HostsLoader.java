package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.AsyncTaskLoader;
import android.content.Context;

import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.utils.DIPairUtils;

public class HostsLoader extends AsyncTaskLoader<List<HostRecordInfo>> {

	public HostsLoader(Context context) {
		super(context);
	}

	@Override
	public List<HostRecordInfo> loadInBackground() {
		return DIPairUtils.getHostList();
	}

	@Override
	protected void onStartLoading() {
		forceLoad();
	}

	@Override
	public void onCanceled(List<HostRecordInfo> data) {
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
