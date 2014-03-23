package com.yugioh.android.fragments;

import android.app.Fragment;
import android.app.FragmentManager;
import android.os.Build;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import com.rarnu.devlib.base.BaseTabFragment;
import com.yugioh.android.R;
import com.yugioh.android.common.MenuIds;

import java.util.List;

public class MainFragment extends BaseTabFragment {

    MenuItem itemSearch;
    MenuItem itemReset;
    SearchFragment sf;
    SearchResultFragment srf;

    public MainFragment() {
        super();
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        sf = new SearchFragment();
        srf = new SearchResultFragment();
        sf.registerSearchResult(srf);
        if (Build.VERSION.SDK_INT < 17) {
            srf.registerParent(this);
        }
    }

    @Override
    public int getBarTitle() {
        return R.string.app_name;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.app_name;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu menu) {
        itemSearch = menu.add(0, MenuIds.MENUID_SEARCH, 98, R.string.search_search);
        itemSearch.setIcon(android.R.drawable.ic_menu_search);
        itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
        itemReset = menu.add(0, MenuIds.MENUID_RESET, 99, R.string.search_reset);
        itemReset.setIcon(android.R.drawable.ic_menu_close_clear_cancel);
        itemReset.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        Bundle bn = new Bundle();
        FragmentManager fm = null;
        if (Build.VERSION.SDK_INT >= 17) {
            fm = getChildFragmentManager();
        } else {
            fm = getFragmentManager();
        }
        if (fm != null) {

            switch (item.getItemId()) {
                case MenuIds.MENUID_SEARCH:
                    if (getCurrentPage() == 0) {
                        bn.putString("data", "search");
                        sf.setNewArguments(bn);
                    } else {
                        setTabPosition(0);
                    }
                    break;
                case MenuIds.MENUID_RESET:
                    bn.putString("data", "reset");
                    sf.setNewArguments(bn);
                    if (getCurrentPage() != 0) {
                        setTabPosition(0);
                    }
                    break;
            }
        }
        return true;
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initFragmentList(List<Fragment> listFragment) {
        listFragment.add(sf);
        listFragment.add(srf);
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
