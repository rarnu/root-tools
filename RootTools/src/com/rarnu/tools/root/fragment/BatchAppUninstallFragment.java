package com.rarnu.tools.root.fragment;

import android.content.Intent;
import android.content.Loader;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ListView;
import android.widget.SearchView;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataBar;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.devlib.component.MutaxReceiver;
import com.rarnu.devlib.component.intf.OnReceiveMessage;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.DataappAdapter;
import com.rarnu.tools.root.common.*;
import com.rarnu.tools.root.loader.BackupLoader;
import com.rarnu.tools.root.service.BatchUninstallService;
import com.rarnu.tools.root.utils.ListUtils;
import com.rarnu.utils.ImageUtils;
import com.rarnu.utils.ResourceUtils;

import java.util.ArrayList;
import java.util.List;

public class BatchAppUninstallFragment extends BaseFragment implements Loader.OnLoadCompleteListener<List<DataappInfo>>, OnReceiveMessage, View.OnClickListener, SearchView.OnQueryTextListener {

    ListView lvBatchApps;
    DataBar barBatchApps;
    DataProgressBar progressBatchApps;
    TextView tvEmptyHint;
    List<DataappInfo> list = new ArrayList<DataappInfo>();
    DataappAdapter adapter = null;
    BackupLoader loader = null;
    MenuItem itemRefresh;
    MenuItem itemSearch;
    MutaxReceiver receiver;
    private Handler hSelectApp = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                showAppSelectedCount();
            }
            super.handleMessage(msg);
        }
    };

    public BatchAppUninstallFragment() {
        super();
        tabTitle = ResourceUtils.getString(R.string.uninstall);
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
        lvBatchApps = (ListView) innerView.findViewById(R.id.lvBatchApps);
        barBatchApps = (DataBar) innerView.findViewById(R.id.barBatchApps);
        progressBatchApps = (DataProgressBar) innerView.findViewById(R.id.progressBatchApps);
        tvEmptyHint = (TextView) innerView.findViewById(R.id.tvEmptyHint);

        adapter = new DataappAdapter(getActivity(), list, hSelectApp, 1);
        lvBatchApps.setAdapter(adapter);
        barBatchApps.setCheckBoxVisible(true);
        loader = new BackupLoader(getActivity());
        receiver = new MutaxReceiver(Actions.ACTION_BATCH_UNINSTALL, Actions.ACTION_BATCH_UNINSTALL_PROGRESS,
                new String[]{Actions.ACTION_BACKUP, Actions.ACTION_RESTORE, Actions.ACTION_BATCH_INSTALL});
        tvEmptyHint.setText(R.string.uninstall_empty);
    }

    @Override
    public void onResume() {
        super.onResume();
        receiver.register(getActivity());
        setUninstallState(false);
    }

    private void setUninstallState(boolean uninstall) {
        adapter.setAdapterCheckable(!uninstall);
        if (uninstall) {
            barBatchApps.setVisibility(View.GONE);
            progressBatchApps.setVisibility(View.VISIBLE);

        } else {
            progressBatchApps.setVisibility(View.GONE);
            ListUtils.setListViewItemSelectedStatus(list, adapter, hSelectApp, false);

        }
    }

    @Override
    public void onPause() {
        receiver.unregister(getActivity());
        super.onPause();
    }

    @Override
    public void initEvents() {
        barBatchApps.getButton1().setOnClickListener(this);
        barBatchApps.getButton2().setOnClickListener(this);
        barBatchApps.getCheckBox().setOnClickListener(this);
        loader.registerListener(0, this);
        receiver.setOnReceiveMessage(this);
    }

    @Override
    public void initLogic() {
        doStartLoad();
    }

    private void doStartLoad() {

        barBatchApps.setVisibility(View.GONE);
        progressBatchApps.setAppName(getString(R.string.loading));
        progressBatchApps.setProgress("");
        progressBatchApps.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_batch_apps;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu menu) {

        itemRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 98, R.string.refresh);
        itemRefresh.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.refresh));
        itemRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

        itemSearch = menu.add(0, MenuItemIds.MENU_SEARCH, 99, R.string.search);
        itemSearch.setIcon(android.R.drawable.ic_menu_search);
        itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        SearchView sv = new SearchView(getActivity());
        sv.setOnQueryTextListener(this);
        itemSearch.setActionView(sv);
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
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onLoadComplete(Loader<List<DataappInfo>> loader, List<DataappInfo> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        adapter.setNewList(list);
        progressBatchApps.setVisibility(View.GONE);

        showAppSelectedCount();
        tvEmptyHint.setVisibility(adapter.getCount() == 0 ? View.VISIBLE : View.GONE);

    }

    @Override
    public void onStateChange(boolean operating) {
        if (!operating) {
            Intent inUninstallService = new Intent(getActivity(), BatchUninstallService.class);
            getActivity().stopService(inUninstallService);
            doStartLoad();
        }
        setUninstallState(operating);
    }

    @Override
    public void onProgress(String name, int position, int total) {
        setUninstallProgress(name, position, total);
    }

    @Override
    public void onMutaxMessage(boolean operating) {
        setOtherProcState(operating);
    }

    private void setUninstallProgress(String name, int position, int total) {
        progressBatchApps.setAppName(getString(R.string.uninstall) + name);
        progressBatchApps.setProgress(String.format("%d / %d", position, total));
    }

    private void setOtherProcState(boolean operating) {
        adapter.setAdapterCheckable(!operating);
        if (operating) {
            barBatchApps.setVisibility(View.GONE);
            progressBatchApps.setVisibility(View.VISIBLE);
            progressBatchApps.setAppName(getString(R.string.mutax_operating));
        } else {
            progressBatchApps.setVisibility(View.GONE);
        }
    }

    private int getAppSelectedCount() {
        return ListUtils.getListViewSelectedCount(list);
    }

    private void showAppSelectedCount() {
        // show appselected count
        try {
            int count = getAppSelectedCount();
            String cap = String.format(getResources().getString(R.string.uninstall_fmt), count);
            barBatchApps.setButton1Text(cap);
            barBatchApps.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
        } catch (Exception e) {

        }
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.barButton1:
                doBatchUninstall();
                break;
            case R.id.barButton2:
                ListUtils.setListViewItemSelectedStatus(list, adapter, hSelectApp, false);
                break;
            case R.id.chkSelAll:
                boolean selected = barBatchApps.getCheckBox().isChecked();
                ListUtils.setListViewItemSelectedStatus(list, adapter, hSelectApp, selected);
                break;
        }
    }

    private void doBatchUninstall() {
        setUninstallState(true);
        List<DataappInfo> listOperate = new ArrayList<DataappInfo>();
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i).checked) {
                listOperate.add(list.get(i));
            }
        }
        ListUtils.setOperateList(listOperate);
        progressBatchApps.setAppName(getString(R.string.uninstalling));
        Intent inUninstallService = new Intent(getActivity(), BatchUninstallService.class);
        inUninstallService.putExtra("command", FragmentNameConst.FN_BATCH_UNINSTALL);
        inUninstallService.putExtra("id", RTConsts.NOTIFY_ID_BATCH_UNINSTALL);
        inUninstallService.putExtra("title", R.string.uninstall);
        inUninstallService.putExtra("desc", R.string.uninstall_ok);
        inUninstallService.putExtra("proc_id", RTConsts.NOTIFY_PROC_BATCH_UNINSTALL);
        inUninstallService.putExtra("proc_title", R.string.uninstall);
        inUninstallService.putExtra("proc_desc", R.string.uninstalling_proc);

        getActivity().startService(inUninstallService);
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        if (adapter != null) {
            adapter.getFilter().filter(newText);
        }
        return false;
    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }
}
