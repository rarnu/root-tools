package com.sbbs.me.android.loader;

import android.content.Context;
import android.util.Log;

import com.rarnu.devlib.base.BaseClassLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeCodeTree;

public class SbbsAndroidRepoLoader extends BaseClassLoader<SbbsMeCodeTree> {
	
	public SbbsAndroidRepoLoader(Context context) {
		super(context);
	}

	public SbbsMeCodeTree loadInBackground() {
		SbbsMeCodeTree tree = null;
		try {
			tree = SbbsMeAPI.getCodeTree("rarnu", "root-tools");
		} catch (Exception e) {
			Log.e("loadInBackground", e.getMessage());
		}
		return tree;
	}
}
