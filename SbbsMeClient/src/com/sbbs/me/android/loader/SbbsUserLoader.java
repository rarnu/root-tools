package com.sbbs.me.android.loader;

import android.content.Context;
import android.util.Log;

import com.rarnu.devlib.base.BaseClassLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeUser;

public class SbbsUserLoader extends BaseClassLoader<SbbsMeUser> {

	private String userId;

	public SbbsUserLoader(Context context) {
		super(context);
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	@Override
	public SbbsMeUser loadInBackground() {
		SbbsMeUser user = null;
		try {
			user = SbbsMeAPI.getUser(userId);
		} catch (Exception e) {
			Log.e("loadInBackground", e.getMessage());
		}
		return user;
	}

}
