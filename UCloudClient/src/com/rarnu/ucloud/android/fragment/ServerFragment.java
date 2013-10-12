package com.rarnu.ucloud.android.fragment;

import android.content.Loader;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ListView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.ucloud.android.R;
import com.rarnu.ucloud.android.adapter.ServerAdapter;
import com.rarnu.ucloud.android.common.MenuIds;
import com.rarnu.ucloud.android.loader.ServerLoader;
import com.rarnu.ucloud.android.pojo.ServerItem;
import com.rarnu.utils.ImageUtils;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;

import java.util.ArrayList;
import java.util.List;

public class ServerFragment extends BaseFragment implements Loader.OnLoadCompleteListener<List<ServerItem>> {

    ListView lvServer;
    MenuItem miRefresh;
    List<ServerItem> list;
    ServerAdapter adapter;
    ServerLoader loader;

    public ServerFragment() {
        super();
        tagText = ResourceUtils.getString(R.string.tag_server_fragment);
        tabTitle = ResourceUtils.getString(R.string.title_server_fragment);
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
        lvServer = (ListView) innerView.findViewById(R.id.lvServer);
        list = new ArrayList<ServerItem>();

        int itemHeight = (int) (UIUtils.getWidth() * 0.345D);

        adapter = new ServerAdapter(getActivity(), list, itemHeight);
        lvServer.setAdapter(adapter);
        loader = new ServerLoader(getActivity());
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
        return R.layout.fragment_server;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu menu) {
        miRefresh = menu.add(0, MenuIds.MENUID_REFRESH, 98, R.string.menu_refresh);
        miRefresh.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.refresh));
        miRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuIds.MENUID_REFRESH:
                loader.startLoading();
                break;
        }
        return true;
    }

    @Override
    public void onGetNewArguments(Bundle bn) {
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onLoadComplete(Loader<List<ServerItem>> loader, List<ServerItem> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
        }
    }
}
