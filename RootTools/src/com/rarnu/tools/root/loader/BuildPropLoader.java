package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.common.BuildPropInfo;
import com.rarnu.tools.root.utils.BuildPropUtils;

import java.util.List;

public class BuildPropLoader extends BaseLoader<BuildPropInfo> {

    public BuildPropLoader(Context context) {
        super(context);
    }

    @Override
    public List<BuildPropInfo> loadInBackground() {
        return BuildPropUtils.getBuildProp();
    }
}
