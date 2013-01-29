package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.tools.root.base.BaseLoader;
import com.rarnu.tools.root.common.SysappInfo;
import com.rarnu.tools.root.utils.ApkUtils;

public class SysappLoader extends BaseLoader<SysappInfo> {

	public SysappLoader(Context context) {
		super(context);
	}

	@Override
	public List<SysappInfo> loadInBackground() {
		return ApkUtils.getSystemApps(getContext());
	}

}
