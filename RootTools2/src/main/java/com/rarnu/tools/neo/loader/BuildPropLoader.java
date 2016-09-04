package com.rarnu.tools.neo.loader;

import android.content.Context;
import com.rarnu.tools.neo.base.BaseLoader;
import com.rarnu.tools.neo.data.BuildPropInfo;
import com.rarnu.tools.neo.utils.BuildPropUtils;

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
