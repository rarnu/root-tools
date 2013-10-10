package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.SearchView;
import android.widget.SearchView.OnQueryTextListener;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.MemProcessAdapter;
import com.rarnu.tools.root.common.MemProcessInfo;
import com.rarnu.tools.root.common.MemoryInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RTConsts;
import com.rarnu.tools.root.fragmentactivity.MemProcessActivity;
import com.rarnu.tools.root.loader.ProcessLoader;
import com.rarnu.tools.root.utils.MemorySpecialList;
import com.rarnu.tools.root.utils.MemoryUtils;
import com.rarnu.utils.ImageUtils;

import java.util.ArrayList;
import java.util.List;

public class MemFragment extends BaseFragment implements OnItemClickListener, OnLoadCompleteListener<List<MemProcessInfo>>, OnQueryTextListener {

    final Handler hShowInfo = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                MemoryInfo info = (MemoryInfo) msg.obj;
                if (info == null) {
                    tvMemoryInfo.setText(String.format(getResources().getString(R.string.memory_usage_fmt), 0, 0, 0, 0));
                } else {
                    tvMemoryInfo.setText(String.format(getResources().getString(R.string.memory_usage_fmt), info.Total, info.Free, info.Shared, info.Buffer));
                }
                progressMemory.setVisibility(View.GONE);

            }
            super.handleMessage(msg);
        }

    };
    final Handler hKillProc = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                doDropCacheT();
            }
            super.handleMessage(msg);
        }
    };
    final Handler hDropCache = new Handler() {

        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                if (menuClean != null) {
                    menuClean.setEnabled(true);
                    menuRefresh.setEnabled(true);
                }
                lvMemory.setEnabled(true);
                progressMemory.setVisibility(View.GONE);

                doStartLoad();
            }
            super.handleMessage(msg);
        }
    };
    ListView lvMemory;
    DataProgressBar progressMemory;
    TextView tvProcessInfo, tvMemoryInfo;
    List<MemProcessInfo> listMemProcessAll = new ArrayList<MemProcessInfo>();
    MemProcessAdapter memProcessAdapter = null;
    ProcessLoader loader = null;
    MenuItem menuRefresh, menuClean;
    MenuItem itemSearch;

    @Override
    public int getBarTitle() {
        return R.string.func5_title;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func5_title_with_path;
    }

    @Override
    public void initComponents() {
        progressMemory = (DataProgressBar) innerView.findViewById(R.id.progressMemory);
        tvProcessInfo = (TextView) innerView.findViewById(R.id.tvProcessInfo);
        tvMemoryInfo = (TextView) innerView.findViewById(R.id.tvMemoryInfo);
        lvMemory = (ListView) innerView.findViewById(R.id.lvMemory);
        memProcessAdapter = new MemProcessAdapter(getActivity(), listMemProcessAll);
        lvMemory.setAdapter(memProcessAdapter);
        loader = new ProcessLoader(getActivity());
    }

    private void doStartLoad() {
        if (isAdded()) {
            progressMemory.setAppName(getString(R.string.loading));
            progressMemory.setVisibility(View.VISIBLE);
        }
        if (menuClean != null) {
            menuClean.setEnabled(false);
            menuRefresh.setEnabled(false);
        }
        loader.startLoading();
    }

    @Override
    public void initLogic() {
        doStartLoad();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_memory;
    }

    @Override
    public void initMenu(Menu menu) {
        itemSearch = menu.add(0, MenuItemIds.MENU_SEARCH, 98, R.string.search);
        itemSearch.setIcon(android.R.drawable.ic_menu_search);
        itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        SearchView sv = new SearchView(getActivity());
        sv.setOnQueryTextListener(this);
        itemSearch.setActionView(sv);

        menuRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 99, R.string.refresh);
        menuRefresh.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.refresh));
        menuRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

        menuClean = menu.add(0, MenuItemIds.MENU_CLEAN, 100, R.string.clean);
        menuClean.setIcon(android.R.drawable.ic_menu_delete);
        menuClean.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_REFRESH:
                doStartLoad();
                break;
            case MenuItemIds.MENU_CLEAN:
                doClean();
                break;
        }
        return true;
    }

    private void showMemoryInfo() {
        try {
            tvProcessInfo.setText(String.format(getResources().getString(R.string.process_count_fmt), (listMemProcessAll == null ? 0 : listMemProcessAll.size())));
            progressMemory.setAppName(getString(R.string.loading));
            progressMemory.setVisibility(View.VISIBLE);

            new Thread(new Runnable() {
                @Override
                public void run() {
                    MemoryInfo info = MemoryUtils.getMemoryInfo();
                    Message msg = new Message();
                    msg.what = 1;
                    msg.obj = info;
                    hShowInfo.sendMessage(msg);
                }
            }).start();
        } catch (Exception e) {

        }
    }

    private void doClean() {

        if (GlobalInstance.killProcessBeforeClean) {
            doKillProcT();
        } else {
            doDropCacheT();
        }
    }

    private void doKillProcT() {
        if (menuClean != null) {
            menuClean.setEnabled(false);
            menuRefresh.setEnabled(false);
        }
        lvMemory.setEnabled(false);
        if (isAdded()) {
            progressMemory.setAppName(getString(R.string.cleaning_memory));
            progressMemory.setVisibility(View.VISIBLE);
        }
        new Thread(new Runnable() {
            @Override
            public void run() {

                List<MemProcessInfo> lstTmp = new ArrayList<MemProcessInfo>();
                lstTmp.addAll(listMemProcessAll);

                if (lstTmp != null && lstTmp.size() != 0) {
                    for (MemProcessInfo info : lstTmp) {
                        // only kill the user applications
                        if (info.appInfo != null) {
                            // exclude list
                            if (MemorySpecialList.inExcludeList(info.NAME) == -1) {
                                MemoryUtils.killProcess(info.PID);
                            }
                        }
                    }
                }
                hKillProc.sendEmptyMessage(1);
            }
        }).start();

    }

    private void doDropCacheT() {

        if (menuClean != null) {
            menuClean.setEnabled(false);
            menuRefresh.setEnabled(false);
        }
        lvMemory.setEnabled(false);
        if (isAdded()) {
            progressMemory.setAppName(getString(R.string.cleaning_memory));
            progressMemory.setVisibility(View.VISIBLE);
        }
        new Thread(new Runnable() {

            @Override
            public void run() {
                MemoryUtils.dropCache();
                hDropCache.sendEmptyMessage(1);

            }
        }).start();
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (resultCode != Activity.RESULT_OK) {
            return;
        }
        switch (requestCode) {
            case RTConsts.REQCODE_MEMORY:
                listMemProcessAll.remove(GlobalInstance.currentMemoryProcess);
                memProcessAdapter.deleteItem(GlobalInstance.currentMemoryProcess);
                tvProcessInfo.setText(String.format(getResources().getString(R.string.process_count_fmt), listMemProcessAll.size()));
                showMemoryInfo();
                break;
        }
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        GlobalInstance.currentMemoryProcess = (MemProcessInfo) lvMemory.getItemAtPosition(position);

        Intent inMem = new Intent(getActivity(), MemProcessActivity.class);
        startActivityForResult(inMem, RTConsts.REQCODE_MEMORY);

    }

    @Override
    public void onLoadComplete(Loader<List<MemProcessInfo>> loader, List<MemProcessInfo> data) {

        listMemProcessAll.clear();
        if (data != null) {
            listMemProcessAll.addAll(data);
        }

        memProcessAdapter.setNewList(listMemProcessAll);

        progressMemory.setVisibility(View.GONE);
        if (menuClean != null) {
            menuClean.setEnabled(true);
            menuRefresh.setEnabled(true);
        }
        showMemoryInfo();

    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        if (memProcessAdapter != null) {
            memProcessAdapter.getFilter().filter(newText);
        }
        return true;
    }

    @Override
    public void initEvents() {
        lvMemory.setOnItemClickListener(this);
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
