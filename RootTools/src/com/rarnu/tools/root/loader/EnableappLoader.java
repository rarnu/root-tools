package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.EnableappInfo;
import com.rarnu.tools.root.utils.ApkUtils;

public class EnableappLoader extends BaseLoader<EnableappInfo> {

	public EnableappLoader(Context context) {
		super(context);
	}

	@Override
	public List<EnableappInfo> loadInBackground() {
		return ApkUtils.getEnabledApplications(getContext());
	}

}
