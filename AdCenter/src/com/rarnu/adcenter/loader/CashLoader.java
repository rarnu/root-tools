package com.rarnu.adcenter.loader;

import android.content.Context;

import com.rarnu.adcenter.api.AdAPI;
import com.rarnu.adcenter.classes.CashItem;
import com.rarnu.devlib.base.BaseClassLoader;

public class CashLoader extends BaseClassLoader<CashItem> {

	private int userId;
	
	public CashLoader(Context context) {
		super(context);
	}
	
	public void setUserId(int userId) {
		this.userId = userId;
	}

	@Override
	public CashItem loadInBackground() {
		return AdAPI.getCash(userId);
	}

}
