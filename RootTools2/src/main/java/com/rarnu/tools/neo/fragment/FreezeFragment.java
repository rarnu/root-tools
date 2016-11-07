package com.rarnu.tools.neo.fragment;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Loader;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.SearchView;
import android.widget.Toast;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.adapter.AppAdapter;
import com.rarnu.tools.neo.api.NativeAPI;
import com.rarnu.tools.neo.base.BaseFragment;
import com.rarnu.tools.neo.comp.LoadingView;
import com.rarnu.tools.neo.data.AppInfo;
import com.rarnu.tools.neo.loader.AppLoader;
import com.rarnu.tools.neo.utils.AppUtils;

import java.util.ArrayList;
import java.util.List;

public class FreezeFragment extends BaseFragment implements
        AdapterView.OnItemClickListener, AdapterView.OnItemLongClickListener, SearchView.OnQueryTextListener {

    private List<AppInfo> list = null;
    private AppAdapter adapter = null;
    private ListView lvApp = null;
    private AppLoader loader = null;
    private SearchView sv = null;
    private MenuItem miSearch = null;
    private LoadingView loading = null;

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        AppInfo item = adapter.getFiltedItem(position);
        threadChangeAppFreeze(item);
    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        adapter.filter(newText);
        return true;
    }

    @Override
    public int getBarTitle() {
        return R.string.freeze_name;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        lvApp = (ListView) innerView.findViewById(R.id.lvApp);
        loading = (LoadingView) innerView.findViewById(R.id.loading);
        list = new ArrayList<>();
        adapter = new AppAdapter(getContext(), list);
        adapter.setShowSwitch(true);
        lvApp.setAdapter(adapter);
        loader = new AppLoader(getContext());
    }

    @Override
    public void initEvents() {
        lvApp.setOnItemClickListener(this);
        lvApp.setOnItemLongClickListener(this);

        loader.registerListener(0, new Loader.OnLoadCompleteListener<List<AppInfo>>() {
            @Override
            public void onLoadComplete(Loader<List<AppInfo>> loader, List<AppInfo> data) {
                list.clear();
                if (data != null) {
                    list.addAll(data);
                }
                adapter.setNewList(list);
                loading.setVisibility(View.GONE);
            }
        });
    }

    @Override
    public void initLogic() {
        loading.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_freeze;
    }

    @Override
    public String getMainActivityName() {
        return null;
    }

    @Override
    public void initMenu(Menu menu) {
        sv = new SearchView(getContext());
        sv.setOnQueryTextListener(this);
        menu.clear();
        miSearch = menu.add(0, 1, 1, R.string.ab_search);
        miSearch.setIcon(android.R.drawable.ic_menu_search);
        miSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
        miSearch.setActionView(sv);
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    private Handler hFreeze = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 0) {
                Toast.makeText(getContext(), R.string.toast_freeze_fail, Toast.LENGTH_SHORT).show();
            }
            adapter.notifyDataSetChanged();
            lvApp.setEnabled(true);
            super.handleMessage(msg);
        }
    };

    private void threadChangeAppFreeze(final AppInfo item) {
        lvApp.setEnabled(false);
        new Thread(new Runnable() {
            @Override
            public void run() {
                boolean newStat = !item.isDisable;
                boolean ret = NativeAPI.freezeApplication(item.packageName, newStat);
                if (ret) {
                    item.isDisable = newStat;
                }
                Message msg = new Message();
                msg.what = ret ? 1 : 0;
                msg.obj = item;
                hFreeze.sendMessage(msg);
            }
        }).start();
    }

    private void showDeleteAppDialog(final AppInfo item, final boolean isSystemRequired) {
        // delete app
        if (isSystemRequired) {
            new AlertDialog.Builder(getContext())
                    .setTitle(R.string.alert_hint)
                    .setMessage(R.string.alert_cannot_delete_app)
                    .setPositiveButton(R.string.alert_ok, null)
                    .show();
        } else {
            new AlertDialog.Builder(getContext())
                    .setTitle(R.string.alert_hint)
                    .setMessage(getString(R.string.alert_delete_app, item.name))
                    .setPositiveButton(R.string.alert_ok, new DialogInterface.OnClickListener() {
                        @Override
                        public void onClick(DialogInterface dialog, int which) {
                            doDeleteApp(item);
                        }
                    })
                    .setNegativeButton(R.string.alert_cancel, null)
                    .show();
        }

    }

    private Handler hDeleteApp = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            switch (msg.what) {
                case 0:
                    // delete fail
                    Toast.makeText(getContext(), R.string.toast_delete_system_app_fail, Toast.LENGTH_SHORT).show();
                    break;
                case 1:
                    // delete succ
                    Toast.makeText(getContext(), R.string.toast_delete_system_app_succ, Toast.LENGTH_SHORT).show();
                    loading.setVisibility(View.VISIBLE);
                    loader.startLoading();
                    break;
            }
            super.handleMessage(msg);
        }
    };

    private void doDeleteApp(final AppInfo item) {
        // delete app
        new Thread(new Runnable() {
            @Override
            public void run() {
                boolean ret = NativeAPI.deleteSystemApp(item.packageName);
                hDeleteApp.sendEmptyMessage(ret ? 1 : 0);
            }
        }).start();
    }

    @Override
    public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
        AppInfo item = adapter.getFiltedItem(position);
        if (item.isSystem) {
            showDeleteAppDialog(item, AppUtils.isAppRequiredBySystem(item.packageName));
        }
        return true;
    }
}
