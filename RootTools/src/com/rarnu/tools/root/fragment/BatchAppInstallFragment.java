package com.rarnu.tools.root.fragment;

import android.content.Intent;
import android.content.Loader;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Html;
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
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.DataappAdapter;
import com.rarnu.tools.root.common.*;
import com.rarnu.tools.root.loader.RestoreLoader;
import com.rarnu.tools.root.service.BatchInstallService;
import com.rarnu.tools.root.utils.ListUtils;
import com.rarnu.utils.ResourceUtils;

import java.util.ArrayList;
import java.util.List;

public class BatchAppInstallFragment extends BaseFragment implements View.OnClickListener, Loader.OnLoadCompleteListener<List<DataappInfo>>, OnReceiveMessage, SearchView.OnQueryTextListener {

    ListView lvBatchApps;
    DataBar barBatchApps;
    DataProgressBar progressBatchApps;
    TextView tvEmptyHint;
    List<DataappInfo> list = new ArrayList<DataappInfo>();
    DataappAdapter adapter = null;
    RestoreLoader loader = null;
    MenuItem itemRefresh;
    SearchView sv;
    MutaxReceiver receiver;
    private Handler hSelectData = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                if (getActivity() != null) {
                    showDataSelectedCount();
                }
            }
            super.handleMessage(msg);
        }
    };

    public BatchAppInstallFragment() {
        super();
        tabTitle = ResourceUtils.getString(R.string.install);
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
        sv = (SearchView) innerView.findViewById(R.id.sv);

        adapter = new DataappAdapter(getActivity(), list, hSelectData, 2);
        lvBatchApps.setAdapter(adapter);
        barBatchApps.setCheckBoxVisible(true);
        loader = new RestoreLoader(getActivity());
        receiver = new MutaxReceiver(Actions.ACTION_BATCH_INSTALL, Actions.ACTION_BATCH_INSTALL_PROGRESS,
                new String[]{Actions.ACTION_BACKUP, Actions.ACTION_RESTORE, Actions.ACTION_BATCH_UNINSTALL});
        tvEmptyHint.setText(Html.fromHtml(getString(R.string.install_empty, GlobalInstance.batchInstallPath)));
    }

    private void setInstallState(boolean install) {
        adapter.setAdapterCheckable(!install);
        if (install) {
            barBatchApps.setVisibility(View.GONE);
            progressBatchApps.setVisibility(View.VISIBLE);

        } else {
            progressBatchApps.setVisibility(View.GONE);
            ListUtils.setListViewItemSelectedStatus(list, adapter, hSelectData, false);

        }
    }

    public void onResume() {
        super.onResume();
        receiver.register(getActivity());
        setInstallState(false);
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
        sv.setOnQueryTextListener(this);
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
        loader.setPath(GlobalInstance.batchInstallPath);
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
        itemRefresh.setIcon(R.drawable.ic_menu_refresh);
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
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.barButton1:
                doBatchInstall();
                break;
            case R.id.barButton2:
                ListUtils.setListViewItemSelectedStatus(list, adapter, hSelectData, false);
                break;
            case R.id.chkSelAll:
                boolean selected = barBatchApps.getCheckBox().isChecked();
                ListUtils.setListViewItemSelectedStatus(list, adapter, hSelectData, selected);
                break;
        }
    }

    @Override
    public void onLoadComplete(Loader<List<DataappInfo>> loader, List<DataappInfo> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
            progressBatchApps.setVisibility(View.GONE);

            showDataSelectedCount();
            tvEmptyHint.setVisibility(adapter.getCount() == 0 ? View.VISIBLE : View.GONE);
        }
    }

    private int getAppSelectedCount() {
        return ListUtils.getListViewSelectedCount(list);
    }

    private void showDataSelectedCount() {
        // show appselected count
        try {
            int count = getAppSelectedCount();
            String cap = String.format(getResources().getString(R.string.install_fmt), count);
            barBatchApps.setButton1Text(cap);
            barBatchApps.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
        } catch (Exception e) {

        }
    }

    @Override
    public void onStateChange(boolean operating) {
        if (!operating) {
            Intent inInstallService = new Intent(getActivity(), BatchInstallService.class);
            getActivity().stopService(inInstallService);
            doStartLoad();
        }
        setInstallState(operating);
    }

    @Override
    public void onProgress(String name, int position, int total) {
        setInstallProgress(name, position, total);
    }

    @Override
    public void onMutaxMessage(boolean operating) {
        setOtherProcState(operating);
    }

    private void setInstallProgress(String name, int position, int total) {
        progressBatchApps.setAppName(getString(R.string.installing) + name);
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

    private void doBatchInstall() {
        setInstallState(true);
        List<DataappInfo> listOperate = new ArrayList<DataappInfo>();
        for (DataappInfo di : list) {
            if (di.checked) {
                listOperate.add(di);
            }
        }
        ListUtils.setOperateList(listOperate);
        progressBatchApps.setAppName(getString(R.string.installing));
        Intent inInstallService = new Intent(getActivity(), BatchInstallService.class);
        inInstallService.putExtra("command", FragmentNameConst.FN_BATCH_INSTALL);
        inInstallService.putExtra("id", RTConsts.NOTIFY_ID_BATCH_INSTALL);
        inInstallService.putExtra("title", R.string.install);
        inInstallService.putExtra("desc", R.string.install_ok);
        inInstallService.putExtra("proc_id", RTConsts.NOTIFY_PROC_BATCH_INSTALL);
        inInstallService.putExtra("proc_title", R.string.install);
        inInstallService.putExtra("proc_desc", R.string.installing_proc);

        getActivity().startService(inInstallService);
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
