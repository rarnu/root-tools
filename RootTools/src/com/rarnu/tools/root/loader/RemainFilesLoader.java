package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.RemainedInfo;
import com.rarnu.tools.root.utils.RemainedFilesUtils;

import java.util.List;

public class RemainFilesLoader extends BaseLoader<RemainedInfo> {
    public RemainFilesLoader(Context context) {
        super(context);
    }

    @Override
    public List<RemainedInfo> loadInBackground() {
        return RemainedFilesUtils.getRemainedFiles(getContext());
    }
}
