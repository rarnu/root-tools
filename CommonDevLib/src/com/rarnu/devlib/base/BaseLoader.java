package com.rarnu.devlib.base;

import java.util.List;

import android.content.AsyncTaskLoader;
import android.content.Context;

public abstract class BaseLoader<T> extends AsyncTaskLoader<List<T>> {

	public BaseLoader(Context context) {
		super(context);
	}

	public abstract List<T> loadInBackground();

	@Override
	protected void onStartLoading() {
		forceLoad();
	}

	@Override
	public void onCanceled(List<T> data) {
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
