package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.tools.root.base.BaseLoader;
import com.rarnu.tools.root.common.CacheInfo;
import com.rarnu.tools.root.utils.CacheUtils;

public class CleanCacheLoader extends BaseLoader<CacheInfo> {

	public CleanCacheLoader(Context context) {
		super(context);
	}

	@Override
	public List<CacheInfo> loadInBackground() {
		return CacheUtils.getCacheList();
	}
}
