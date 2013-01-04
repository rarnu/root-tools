package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.AsyncTaskLoader;
import android.content.Context;

import com.rarnu.tools.root.common.MemProcessInfo;
import com.rarnu.tools.root.utils.ProcessUtils;

public class ProcessLoader extends AsyncTaskLoader<List<MemProcessInfo>> {

	public ProcessLoader(Context context) {
		super(context);
	}

	@Override
	public List<MemProcessInfo> loadInBackground() {
		return ProcessUtils.getUserProcessList();
	}

	@Override
	protected void onStartLoading() {
		forceLoad();
	}

	@Override
	public void onCanceled(List<MemProcessInfo> data) {
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
