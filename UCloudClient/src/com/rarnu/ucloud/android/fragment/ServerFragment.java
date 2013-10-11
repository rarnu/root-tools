package com.rarnu.ucloud.android.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ListView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.ucloud.android.R;
import com.rarnu.ucloud.android.common.MenuIds;
import com.rarnu.utils.ImageUtils;
import com.rarnu.utils.ResourceUtils;

public class ServerFragment extends BaseFragment {

    ListView lvServer;
    MenuItem miRefresh;

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
    }

    @Override
    public void initEvents() {
    }

    @Override
    public void initLogic() {
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
                // TODO: refresh
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
}
