package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.*;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.SearchView.OnQueryTextListener;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.SysappAdapter;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.common.SysappInfo;
import com.rarnu.tools.root.fragmentactivity.SysappDetailActivity;
import com.rarnu.tools.root.fragmentactivity.SysappSelectApkActivity;
import com.rarnu.tools.root.loader.SysappLoader;
import com.rarnu.tools.root.utils.ApkUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class SysappFragment extends BaseFragment implements OnQueryTextListener, OnItemClickListener, OnLoadCompleteListener<List<SysappInfo>> {

    final Handler hInstall = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1 && getActivity() != null) {
                Toast.makeText(getActivity(), (msg.arg1 == 1 ? R.string.install_ok : R.string.install_fail), Toast.LENGTH_LONG).show();
                progressSysapp.setVisibility(View.GONE);
                doStartLoad();
            }
            super.handleMessage(msg);
        }
    };
    DataProgressBar progressSysapp;
    ListView lvSysApp;
    List<SysappInfo> listSysappAll = new ArrayList<SysappInfo>();
    SysappAdapter sysappAdapter = null;
    SysappLoader loader = null;
    SearchView sv;
    MenuItem itemAdd;
    MenuItem itemRefresh;

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_ADD:
                Intent inSelect = new Intent(getActivity(), SysappSelectApkActivity.class);
                startActivityForResult(inSelect, RTConsts.REQCODE_SYSAPP_SELECT);
                break;
            case MenuItemIds.MENU_REFRESH:
                doStartLoad();
                break;
        }
        return true;
    }

    protected void doStartLoad() {
        progressSysapp.setAppName(getString(R.string.loading));
        progressSysapp.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public int getBarTitle() {
        return R.string.func1_title;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func1_title_with_path;
    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        if (sysappAdapter != null) {
            sysappAdapter.getFilter().filter(newText);
        }
        return true;
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        GlobalInstance.currentSysapp = (SysappInfo) lvSysApp.getItemAtPosition(position);
        Intent inApp = new Intent(getActivity(), SysappDetailActivity.class);
        startActivityForResult(inApp, RTConsts.REQCODE_SYSAPP);
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (resultCode != Activity.RESULT_OK) {
            return;
        }
        switch (requestCode) {
            case RTConsts.REQCODE_SYSAPP:
                boolean needRefresh = data.getBooleanExtra("needRefresh", false);
                if (needRefresh) {
                    listSysappAll.remove(GlobalInstance.currentSysapp);
                    sysappAdapter.deleteItem(GlobalInstance.currentSysapp);
                }
                break;
            case RTConsts.REQCODE_SYSAPP_SELECT:
                String apkPath = data.getStringExtra("path");
                confirmInstall(apkPath);
                break;
        }
    }

    private void confirmInstall(final String apkPath) {
        File apk = new File(apkPath);
        if (!apk.exists()) {
            return;
        }
        if (!apkPath.equals("")) {
            final CheckBox chkInstallToPrivate = new CheckBox(getActivity());
            chkInstallToPrivate.setChecked(false);
            chkInstallToPrivate.setText(R.string.install_to_private);
            new AlertDialog.Builder(getActivity())
                    .setTitle(R.string.hint)
                    .setMessage(String.format(getResources().getString(R.string.install_apk), apk.getName()))
                    .setView(Build.VERSION.SDK_INT >= 19 ? chkInstallToPrivate : null)
                    .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                        @Override
                        public void onClick(DialogInterface dialog, int which) {
                            doInstallSystemApp(apkPath, chkInstallToPrivate.isChecked());
                        }
                    })
                    .setNegativeButton(R.string.cancel, null)
                    .show();
        }
    }

    private void doInstallSystemApp(final String path, boolean isPrivate) {
        progressSysapp.setAppName(getString(R.string.installing_system_app));
        progressSysapp.setVisibility(View.VISIBLE);

        ApkUtils.installSystemApp(getActivity(), path, isPrivate, hInstall);
    }

    @Override
    public void onLoadComplete(Loader<List<SysappInfo>> loader, List<SysappInfo> data) {
        listSysappAll.clear();
        if (data != null) {
            listSysappAll.addAll(data);
        }
        if (getActivity() != null) {
            sysappAdapter.setNewList(listSysappAll);
            progressSysapp.setVisibility(View.GONE);
        }
    }

    @Override
    public void initComponents() {
        progressSysapp = (DataProgressBar) innerView.findViewById(R.id.progressSysapp);
        lvSysApp = (ListView) innerView.findViewById(R.id.lvSysApp);
        sysappAdapter = new SysappAdapter(getActivity(), listSysappAll);
        lvSysApp.setAdapter(sysappAdapter);
        loader = new SysappLoader(getActivity());
        sv = (SearchView) innerView.findViewById(R.id.sv);

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_sysapp;
    }

    @Override
    public void initMenu(Menu menu) {

        itemAdd = menu.add(0, MenuItemIds.MENU_ADD, 98, R.string.add);
        itemAdd.setIcon(android.R.drawable.ic_menu_add);
        itemAdd.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

        itemRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 99, R.string.refresh);
        itemRefresh.setIcon(R.drawable.ic_menu_refresh);
        itemRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public void initLogic() {
        doStartLoad();
    }

    @Override
    public void initEvents() {
        lvSysApp.setOnItemClickListener(this);
        loader.registerListener(0, this);
        sv.setOnQueryTextListener(this);
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
