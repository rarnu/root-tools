package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.AsyncTaskLoader;
import android.content.Context;

import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.common.RecommandInfo;

public class RecommandLoader extends AsyncTaskLoader<List<RecommandInfo>> {

	public RecommandLoader(Context context) {
		super(context);
	}

	@Override
	public List<RecommandInfo> loadInBackground() {
		return MobileApi.getRecommand();
	}

	@Override
	protected void onStartLoading() {
		forceLoad();
	}

	@Override
	public void onCanceled(List<RecommandInfo> data) {
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
