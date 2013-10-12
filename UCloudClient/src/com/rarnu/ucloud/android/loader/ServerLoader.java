package com.rarnu.ucloud.android.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.ucloud.android.pojo.ServerItem;

import java.util.ArrayList;
import java.util.List;

public class ServerLoader extends BaseLoader<ServerItem> {

    private String userToken;

    public ServerLoader(Context context) {
        super(context);
    }

    public void setUserToken(String token) {
        this.userToken = token;
    }

    @Override
    public List<ServerItem> loadInBackground() {

        List<ServerItem> list = new ArrayList<ServerItem>();
        for (int i = 0; i < 10; i++) {
            ServerItem item = new ServerItem();
            item.name = String.format("Demo server No.%d", i + 1);
            item.id = i;
            item.runningTime = ((i + 1) * 10);
            item.state = 0;
            item.loadLineUrl = "";
            list.add(item);
        }
        return list;
    }
}
