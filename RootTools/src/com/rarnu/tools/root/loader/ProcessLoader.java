package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.tools.root.base.BaseLoader;
import com.rarnu.tools.root.common.MemProcessInfo;
import com.rarnu.tools.root.utils.ProcessUtils;

public class ProcessLoader extends BaseLoader<MemProcessInfo> {

	public ProcessLoader(Context context) {
		super(context);
	}

	@Override
	public List<MemProcessInfo> loadInBackground() {
		return ProcessUtils.getUserProcessList();
	}

}
