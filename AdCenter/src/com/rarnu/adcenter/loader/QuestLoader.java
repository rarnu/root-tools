package com.rarnu.adcenter.loader;

import android.content.Context;

import com.rarnu.adcenter.api.AdAPI;
import com.rarnu.adcenter.classes.QuestItem;
import com.rarnu.devlib.base.BaseClassLoader;

public class QuestLoader extends BaseClassLoader<QuestItem> {

	private int adId;

	public QuestLoader(Context context) {
		super(context);
	}

	public void setAdId(int adId) {
		this.adId = adId;
	}

	@Override
	public QuestItem loadInBackground() {
		return AdAPI.getQuest(adId);
	}

}
