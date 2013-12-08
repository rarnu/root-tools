package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ListView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.utils.ImageUtils;

public class FirewallFragment extends BaseFragment {

    ListView lvFirewall;
    DataProgressBar progressFirewall;

    MenuItem miRefresh;
    MenuItem miApply;

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

    }

    @Override
    public void initEvents() {

    }

    @Override
    public void initLogic() {

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_firewall;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        miApply = menu.add(0, MenuItemIds.MENU_APPLY, 99, R.string.save);
        miApply.setIcon(android.R.drawable.ic_menu_save);
        miApply.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

        miRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 100, R.string.refresh);
        miRefresh.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.refresh));
        miRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_APPLY:
                // TODO: apply iptables
                break;
            case MenuItemIds.MENU_REFRESH:
                // TODO: refresh app list
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
