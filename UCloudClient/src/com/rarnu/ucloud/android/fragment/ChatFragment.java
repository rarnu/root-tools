package com.rarnu.ucloud.android.fragment;

import android.content.Loader;
import android.os.Bundle;
import android.view.Menu;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.ucloud.android.R;
import com.rarnu.ucloud.android.adapter.ChatAdpater;
import com.rarnu.ucloud.android.loader.ChatLoader;
import com.rarnu.ucloud.android.pojo.ChatItem;
import com.rarnu.utils.ResourceUtils;

import java.util.ArrayList;
import java.util.List;

public class ChatFragment extends BaseFragment implements Loader.OnLoadCompleteListener<List<ChatItem>> {

    ChatLoader loader;
    TextView tvChatter;
    ListView lvChat;
    EditText etChat;
    Button btnSendChat;
    List<ChatItem> list;
    ChatAdpater adapter;

    public ChatFragment() {
        super();
        tagText = ResourceUtils.getString(R.string.tag_chat_fragment);
        tabTitle = ResourceUtils.getString(R.string.title_chat_fragment);
    }

    @Override
    public int getBarTitle() {
        return 0;
    }

    @Override
    public int getBarTitleWithPath() {
        return 0;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        loader = new ChatLoader(getActivity());
        tvChatter = (TextView) innerView.findViewById(R.id.tvChatter);
        lvChat = (ListView) innerView.findViewById(R.id.lvChat);
        etChat = (EditText) innerView.findViewById(R.id.etChat);
        btnSendChat = (Button) innerView.findViewById(R.id.btnSendChat);
        list = new ArrayList<ChatItem>();
        adapter = new ChatAdpater(getActivity(), list);
        lvChat.setAdapter(adapter);

    }

    @Override
    public void initEvents() {
        loader.registerListener(0, this);
    }

    @Override
    public void initLogic() {
        // loader.setUserToken(token);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_chat;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu menu) {
    }

    @Override
    public void onGetNewArguments(Bundle bn) {
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onLoadComplete(Loader<List<ChatItem>> loader, List<ChatItem> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
        }
    }
}
