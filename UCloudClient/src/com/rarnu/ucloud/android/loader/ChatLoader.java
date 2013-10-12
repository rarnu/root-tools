package com.rarnu.ucloud.android.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.ucloud.android.pojo.ChatItem;

import java.util.ArrayList;
import java.util.List;

public class ChatLoader extends BaseLoader<ChatItem>{

    private String userToken;


    // demo data
    private String[] strDemo = new String[] {"测试","测试","测试","测试","测试","测试","测试","测试","测试","测试"};

    public ChatLoader(Context context) {
        super(context);
    }

    public void setUserToken(String token) {
        this.userToken = token;
    }

    @Override
    public List<ChatItem> loadInBackground() {
        List<ChatItem> list = new ArrayList<ChatItem>();
        for (int i=0; i<10; i++) {
            ChatItem item = new ChatItem();
            item.id = i;
            item.fromChatter = (i%2==0);
            item.text = strDemo[i];
            list.add(item);
        }
        return list;
    }
}
