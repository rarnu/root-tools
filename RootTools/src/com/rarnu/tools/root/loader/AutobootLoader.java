package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.AutobootInfo;
import com.rarnu.tools.root.utils.AutobootUtils;
import com.rarnu.utils.UIUtils;

import java.util.List;

public class AutobootLoader extends BaseLoader<AutobootInfo> {

    public AutobootLoader(Context context) {
        super(context);
    }

    @Override
    public List<AutobootInfo> loadInBackground() {
        return AutobootUtils.getAuobootApps(getContext(), UIUtils.getDM());
    }
}
