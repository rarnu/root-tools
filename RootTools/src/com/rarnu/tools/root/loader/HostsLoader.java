package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.utils.DIPairUtils;

public class HostsLoader extends BaseLoader<HostRecordInfo> {

	public HostsLoader(Context context) {
		super(context);
	}

	@Override
	public List<HostRecordInfo> loadInBackground() {
		return DIPairUtils.getHostList();
	}

}
