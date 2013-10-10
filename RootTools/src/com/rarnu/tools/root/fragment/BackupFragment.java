package com.rarnu.tools.root.fragment;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ListView;
import android.widget.SearchView;
import android.widget.SearchView.OnQueryTextListener;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataBar;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.DataappAdapter;
import com.rarnu.tools.root.common.*;
import com.rarnu.tools.root.loader.BackupLoader;
import com.rarnu.tools.root.receiver.MutaxReceiver;
import com.rarnu.tools.root.receiver.MutaxReceiver.OnReceiveMessage;
import com.rarnu.tools.root.service.DataBackupService;
import com.rarnu.tools.root.utils.ListUtils;
import com.rarnu.utils.ImageUtils;

import java.util.ArrayList;
import java.util.List;

public class BackupFragment extends BaseFragment implements OnClickListener, OnLoadCompleteListener<List<DataappInfo>>, OnQueryTextListener, OnReceiveMessage {

    ListView lvData;
    DataBar barData;
    DataProgressBar progressData;
    TextView tvEmptyHint;
    List<DataappInfo> listDataappAll = new ArrayList<DataappInfo>();
    DataappAdapter dataappAdapter = null;
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

    @Override
    public int getBarTitle() {
        return R.string.func3_title;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func3_title_with_path;
    }

    @Override
    public void initComponents() {
        barData = (DataBar) innerView.findViewById(R.id.barData);
        progressData = (DataProgressBar) innerView.findViewById(R.id.progressData);
        lvData = (ListView) innerView.findViewById(R.id.lvData);
        tvEmptyHint = (TextView) innerView.findViewById(R.id.tvEmptyHint);

        dataappAdapter = new DataappAdapter(getActivity(), listDataappAll, hSelectApp, 1);
        lvData.setAdapter(dataappAdapter);
        barData.setCheckBoxVisible(true);
        loader = new BackupLoader(getActivity());
        receiver = new MutaxReceiver(Actions.ACTION_BACKUP, Actions.ACTION_BACKUP_PROGRESS, new String[]{Actions.ACTION_RESTORE});

    }

    @Override
    public void initLogic() {
        doStartLoad();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_backup;
    }

    @Override
    public void onResume() {
        super.onResume();
        receiver.register(getActivity());
        setBackupState(false);
    }

    @Override
    public void onPause() {
        receiver.unregister(getActivity());
        super.onPause();
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

    private int getAppSelectedCount() {
        return ListUtils.getListViewSelectedCount(listDataappAll);
    }

    private void showAppSelectedCount() {
        // show appselected count
        try {
            int count = getAppSelectedCount();
            String cap = String.format(getResources().getString(R.string.btn_backup), count);
            barData.setButton1Text(cap);
            barData.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
        } catch (Exception e) {

        }
    }

    private void doBackup() {
        setBackupState(true);
        List<DataappInfo> listOperate = new ArrayList<DataappInfo>();
        for (int i = 0; i < listDataappAll.size(); i++) {
            if (listDataappAll.get(i).checked) {
                listOperate.add(listDataappAll.get(i));
            }
        }
        ListUtils.setOperateList(listOperate);
        progressData.setAppName(getString(R.string.backuping));
        Intent inBackupService = new Intent(getActivity(), DataBackupService.class);
        inBackupService.putExtra("command", FragmentNameConst.FN_BACKUP);
        inBackupService.putExtra("id", RTConsts.NOTIFY_ID_BACKUP);
        inBackupService.putExtra("title", R.string.func3_title);
        inBackupService.putExtra("desc", R.string.backup_ok);
        inBackupService.putExtra("proc_id", RTConsts.NOTIFY_PROC_BACKUP);
        inBackupService.putExtra("proc_title", R.string.func3_title);
        inBackupService.putExtra("proc_desc", R.string.backuping_proc);

        getActivity().startService(inBackupService);
    }

    private void setBackupState(boolean backup) {
        dataappAdapter.setAdapterCheckable(!backup);
        if (backup) {
            barData.setVisibility(View.GONE);
            progressData.setVisibility(View.VISIBLE);

        } else {
            progressData.setVisibility(View.GONE);
            ListUtils.setListViewItemSelectedStatus(listDataappAll, dataappAdapter, hSelectApp, false);
        }
    }

    private void setOtherProcState(boolean restore) {
        dataappAdapter.setAdapterCheckable(!restore);
        if (restore) {
            barData.setVisibility(View.GONE);
            progressData.setVisibility(View.VISIBLE);
            progressData.setAppName(getString(R.string.mutax_restore));
        } else {
            progressData.setVisibility(View.GONE);
        }
    }

    private void setBackupProgress(String name, int position, int total) {
        progressData.setAppName(getString(R.string.backuping) + name);
        progressData.setProgress(String.format("%d / %d", position, total));
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.barButton1:
                doBackup();
                break;
            case R.id.barButton2:
                ListUtils.setListViewItemSelectedStatus(listDataappAll, dataappAdapter, hSelectApp, false);
                break;
            case R.id.chkSelAll:
                boolean selected = barData.getCheckBox().isChecked();
                ListUtils.setListViewItemSelectedStatus(listDataappAll, dataappAdapter, hSelectApp, selected);
                break;
        }

    }

    private void doStartLoad() {

        barData.setVisibility(View.GONE);
        progressData.setAppName(getString(R.string.loading));
        progressData.setProgress("");
        progressData.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public void onLoadComplete(Loader<List<DataappInfo>> loader, List<DataappInfo> data) {

        listDataappAll.clear();
        if (data != null) {
            listDataappAll.addAll(data);
        }
        dataappAdapter.setNewList(listDataappAll);
        progressData.setVisibility(View.GONE);

        showAppSelectedCount();
        tvEmptyHint.setVisibility(dataappAdapter.getCount() == 0 ? View.VISIBLE : View.GONE);

    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        if (dataappAdapter != null) {
            dataappAdapter.getFilter().filter(newText);
        }
        return true;
    }

    @Override
    public void onStateChange(boolean operating) {
        if (!operating) {
            Intent inBackupService = new Intent(getActivity(), DataBackupService.class);
            getActivity().stopService(inBackupService);
        }
        setBackupState(operating);

    }

    @Override
    public void onProgress(String name, int position, int total) {
        setBackupProgress(name, position, total);
    }

    @Override
    public void onMutaxMessage(boolean operating) {
        setOtherProcState(operating);

    }

    @Override
    public void initEvents() {
        barData.getButton1().setOnClickListener(this);
        barData.getButton2().setOnClickListener(this);
        barData.getCheckBox().setOnClickListener(this);
        loader.registerListener(0, this);
        receiver.setOnReceiveMessage(this);

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
