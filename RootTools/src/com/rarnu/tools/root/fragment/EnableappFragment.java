package com.rarnu.tools.root.fragment;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.*;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.SearchView.OnQueryTextListener;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.EnableappAdapter;
import com.rarnu.tools.root.common.EnableappInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.loader.EnableappLoader;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.ComponentUtils;
import com.rarnu.utils.ImageUtils;

import java.util.ArrayList;
import java.util.List;

public class EnableappFragment extends BaseFragment implements OnItemLongClickListener, OnLoadCompleteListener<List<EnableappInfo>>, OnQueryTextListener {

    ListView lvEnableApp;
    DataProgressBar progressEnableapp;
    TextView tvOperateHint;
    boolean enableappLoading = false;
    EnableappAdapter enableappAdapter;
    List<EnableappInfo> listEnableappAll = new ArrayList<EnableappInfo>();
    EnableappLoader loader = null;
    MenuItem itemSearch;
    MenuItem itemRefresh;

    @Override
    public int getBarTitle() {
        return R.string.func2_title;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func2_title_with_path;
    }

    @Override
    public void initComponents() {

        lvEnableApp = (ListView) innerView.findViewById(R.id.lvEnableApp);
        progressEnableapp = (DataProgressBar) innerView.findViewById(R.id.progressEnableapp);
        tvOperateHint = (TextView) innerView.findViewById(R.id.tvOperateHint);

        enableappAdapter = new EnableappAdapter(getActivity(), listEnableappAll);
        lvEnableApp.setAdapter(enableappAdapter);
        loader = new EnableappLoader(getActivity());

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_enableapp;
    }

    @Override
    public void initMenu(Menu menu) {
        itemSearch = menu.add(0, MenuItemIds.MENU_SEARCH, 98, R.string.search);
        itemSearch.setIcon(android.R.drawable.ic_menu_search);
        itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        SearchView sv = new SearchView(getActivity());
        sv.setOnQueryTextListener(this);
        itemSearch.setActionView(sv);

        itemRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 99, R.string.refresh);
        itemRefresh.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.refresh));
        itemRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_REFRESH:
                doStartLoad();
                break;
        }
        return true;
    }

    @Override
    public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
        final EnableappInfo info = (EnableappInfo) lvEnableApp.getItemAtPosition(position);
        if (info.type == 0) {
            switchSystemApp(info);
        } else if (info.type == 1) {
            uninstallUserApp(info);

        } else {
            Toast.makeText(getActivity(), R.string.cannot_change_package_status, Toast.LENGTH_LONG).show();
        }
        return true;
    }

    private void switchSystemApp(EnableappInfo info) {
        boolean ret = false;
        if (info.enabled) {
            ret = ComponentUtils.doDisableApplication(info);
            if (ret) {
                info.enabled = false;
            }
        } else {
            ret = ComponentUtils.doEnableApplication(info);
            if (ret) {
                info.enabled = true;
            }
        }
        if (ret) {
            enableappAdapter.sort();
            enableappAdapter.notifyDataSetChanged();
        } else {
            Toast.makeText(getActivity(), R.string.change_package_status_fail, Toast.LENGTH_LONG).show();
        }
    }

    private void uninstallUserApp(final EnableappInfo info) {
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.func2_title)
                .setMessage(R.string.data_app_uninstall)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {

                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        if (ApkUtils.uninstallApk(info.info.packageName)) {
                            enableappAdapter.deleteItem(info);
                        } else {
                            Toast.makeText(getActivity(), R.string.cannot_uninstall_package, Toast.LENGTH_LONG).show();
                        }

                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    private void doStartLoad() {
        tvOperateHint.setVisibility(View.GONE);
        progressEnableapp.setAppName(getString(R.string.loading));
        progressEnableapp.setProgress("");
        progressEnableapp.setVisibility(View.VISIBLE);
        enableappLoading = true;
        loader.startLoading();
    }

    @Override
    public void onLoadComplete(Loader<List<EnableappInfo>> loader, List<EnableappInfo> data) {

        listEnableappAll.clear();
        if (data != null) {
            listEnableappAll.addAll(data);
        }
        enableappAdapter.setNewList(listEnableappAll);
        progressEnableapp.setVisibility(View.GONE);
        tvOperateHint.setVisibility(View.VISIBLE);
        enableappLoading = false;

    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        if (enableappAdapter != null) {
            enableappAdapter.getFilter().filter(newText);
        }
        return false;
    }

    @Override
    public void initLogic() {
        doStartLoad();

    }

    @Override
    public void initEvents() {
        lvEnableApp.setOnItemLongClickListener(this);
        loader.registerListener(0, this);
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
