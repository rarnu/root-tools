package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.CacheInfo;
import com.rarnu.tools.root.utils.CacheUtils;

import java.util.List;

public class CleanCacheLoader extends BaseLoader<CacheInfo> {

    public CleanCacheLoader(Context context) {
        super(context);
    }

    @Override
    public List<CacheInfo> loadInBackground() {
        List<CacheInfo> list = null;
        if (getContext() != null) {
            list = CacheUtils.getCacheList();
        }
        return list;
    }
}
