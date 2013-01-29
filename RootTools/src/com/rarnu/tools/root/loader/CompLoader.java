package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.Context;
import android.content.pm.PackageInfo;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.base.BaseLoader;

public class CompLoader extends BaseLoader<PackageInfo> {

	public CompLoader(Context context) {
		super(context);
	}

	@Override
	public List<PackageInfo> loadInBackground() {
		return GlobalInstance.pm.getInstalledPackages(0);
	}

}
