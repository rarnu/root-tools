package com.rarnu.vim.emotion.loader;

import java.util.List;

import android.content.AsyncTaskLoader;
import android.content.Context;

import com.rarnu.vim.emotion.Global;
import com.rarnu.vim.emotion.database.EmotionInfo;

public class HistoryLoader extends AsyncTaskLoader<List<EmotionInfo>> {

	public HistoryLoader(Context context) {
		super(context);
		
	}

	@Override
	public List<EmotionInfo> loadInBackground() {
		return Global.database.queryHistory();
	}
	
	@Override
	protected void onStartLoading() {
		forceLoad();
	}

	@Override
	public void onCanceled(List<EmotionInfo> data) {
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
