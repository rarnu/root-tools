package com.rarnu.ucloud.android.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.ucloud.android.pojo.FlowItem;

import java.util.List;

public class FlowLoader extends BaseLoader<FlowItem> {

    private String userToken;

    public FlowLoader(Context context) {
        super(context);
    }

    public void setUserToken(String token) {
        this.userToken = token;
    }

    @Override
    public List<FlowItem> loadInBackground() {
        return null;
    }
}
