package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.DiskInfo;
import com.rarnu.tools.root.utils.DiskUtils;

public class DiskInfoLoader extends BaseLoader<DiskInfo> {

	public DiskInfoLoader(Context context) {
		super(context);
	}

	@Override
	public List<DiskInfo> loadInBackground() {
		return DiskUtils.getDiskInfoList();
	}

}
