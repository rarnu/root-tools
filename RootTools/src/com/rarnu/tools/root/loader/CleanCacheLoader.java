package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.AsyncTaskLoader;
import android.content.Context;

import com.rarnu.tools.root.common.CacheInfo;
import com.rarnu.tools.root.utils.CacheUtils;

public class CleanCacheLoader extends AsyncTaskLoader<List<CacheInfo>> {

	public CleanCacheLoader(Context context) {
		super(context);
	}

	@Override
	public List<CacheInfo> loadInBackground() {
		return CacheUtils.getCacheList();
	}

	@Override
	protected void onStartLoading() {
		forceLoad();
	}

	@Override
	public void onCanceled(List<CacheInfo> data) {
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
