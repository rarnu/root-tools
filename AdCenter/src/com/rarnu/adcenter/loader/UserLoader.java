package com.rarnu.adcenter.loader;

import android.content.Context;

import com.rarnu.adcenter.api.AdAPI;
import com.rarnu.adcenter.classes.UserItem;
import com.rarnu.devlib.base.BaseClassLoader;

public class UserLoader extends BaseClassLoader<UserItem> {

	private int userId;
	
	public UserLoader(Context context) {
		super(context);
	}
	
	public void setUserId(int userId) {
		this.userId = userId;
	}

	@Override
	public UserItem loadInBackground() {
		return AdAPI.getUser(userId);
	}

}
