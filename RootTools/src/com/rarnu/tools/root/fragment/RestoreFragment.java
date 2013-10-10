package com.rarnu.tools.root.fragment;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Html;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.ListView;
import android.widget.SearchView;
import android.widget.SearchView.OnQueryTextListener;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataBar;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.DataappAdapter;
import com.rarnu.tools.root.common.*;
import com.rarnu.tools.root.loader.RestoreLoader;
import com.rarnu.tools.root.receiver.MutaxReceiver;
import com.rarnu.tools.root.receiver.MutaxReceiver.OnReceiveMessage;
import com.rarnu.tools.root.service.DataRestoreService;
import com.rarnu.tools.root.utils.ApkUtils;
import com.rarnu.tools.root.utils.ListUtils;
import com.rarnu.utils.ImageUtils;

import java.util.ArrayList;
import java.util.List;

public class RestoreFragment extends BaseFragment implements OnItemLongClickListener, OnClickListener, OnLoadCompleteListener<List<DataappInfo>>, OnQueryTextListener, OnReceiveMessage {

    final Handler hDeleteBackup = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                DataappInfo item = (DataappInfo) msg.obj;
                listBackDataappAll.remove(item);
                backDataappAdapter.deleteItem(item);
                progressBackData.setVisibility(View.GONE);
                lvBackData.setEnabled(true);
            }
            super.handleMessage(msg);
        }
    };
    ListView lvBackData;
    DataBar barBackData;
    DataProgressBar progressBackData;
    TextView tvEmptyHint;
    List<DataappInfo> listBackDataappAll = new ArrayList<DataappInfo>();
    DataappAdapter backDataappAdapter = null;
    RestoreLoader loader = null;
    MenuItem itemRefresh;
    MenuItem itemSearch;
    MutaxReceiver receiver;
    private Handler hSelectData = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                showDataSelectedCount();
            }
            super.handleMessage(msg);
        }
    };

    @Override
    public int getBarTitle() {
        return R.string.func3p_title;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func3p_title_with_path;
    }

    @Override
    public void onResume() {
        super.onResume();
        receiver.register(getActivity());
        setRestoreState(false);
    }

    public void onPause() {
        receiver.unregister(getActivity());
        super.onPause();
    }

    @Override
    public void initComponents() {
        barBackData = (DataBar) innerView.findViewById(R.id.barBackData);
        progressBackData = (DataProgressBar) innerView.findViewById(R.id.progressBackData);
        lvBackData = (ListView) innerView.findViewById(R.id.lvBackData);
        tvEmptyHint = (TextView) innerView.findViewById(R.id.tvEmptyHint);
        tvEmptyHint.setText(Html.fromHtml(getString(R.string.restore_empty)));
        backDataappAdapter = new DataappAdapter(getActivity(), listBackDataappAll, hSelectData, 2);
        lvBackData.setAdapter(backDataappAdapter);
        barBackData.setCheckBoxVisible(true);
        loader = new RestoreLoader(getActivity());
        receiver = new MutaxReceiver(Actions.ACTION_RESTORE, Actions.ACTION_RESTORE_PROGRESS, new String[]{Actions.ACTION_BACKUP});

    }

    @Override
    public void initLogic() {
        doStartLoad();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_restore;
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

    private int getDataSelectedCount() {
        return ListUtils.getListViewSelectedCount(listBackDataappAll);
    }

    private void showDataSelectedCount() {
        try {
            int count = getDataSelectedCount();
            String cap = String.format(
                    getResources().getString(R.string.btn_restore), count);
            barBackData.setButton1Text(cap);
            barBackData.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
        } catch (Exception e) {

        }
    }

    private void confirmDeleteBackup(final DataappInfo item) {

        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(R.string.confirm_delete_backup)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {

                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        doDeleteBackupT(item);

                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();

    }

    private void doDeleteBackupT(final DataappInfo item) {
        lvBackData.setEnabled(false);
        progressBackData.setAppName(getString(R.string.deleting));
        progressBackData.setVisibility(View.VISIBLE);

        new Thread(new Runnable() {
            @Override
            public void run() {
                ApkUtils.deleteBackupData(item.info.packageName, GlobalInstance.backupPath);
                Message msg = new Message();
                msg.what = 1;
                msg.obj = item;
                hDeleteBackup.sendMessage(msg);
            }
        }).start();
    }

    private void setRestoreState(boolean resotre) {
        backDataappAdapter.setAdapterCheckable(!resotre);
        if (resotre) {
            barBackData.setVisibility(View.GONE);
            progressBackData.setVisibility(View.VISIBLE);
            lvBackData.setOnItemLongClickListener(null);
        } else {
            progressBackData.setVisibility(View.GONE);
            ListUtils.setListViewItemSelectedStatus(listBackDataappAll, backDataappAdapter, hSelectData, false);
            lvBackData.setOnItemLongClickListener(this);
        }
    }

    private void setOtherProcState(boolean backup) {
        backDataappAdapter.setAdapterCheckable(!backup);
        if (backup) {
            barBackData.setVisibility(View.GONE);
            progressBackData.setVisibility(View.VISIBLE);
            progressBackData.setAppName(getString(R.string.mutax_backup));
            lvBackData.setOnItemLongClickListener(null);
        } else {
            progressBackData.setVisibility(View.GONE);
            lvBackData.setOnItemLongClickListener(this);
        }
    }

    private void setRestoreProgress(String name, int position, int total) {
        progressBackData.setAppName(getString(R.string.restoring) + name);
        progressBackData.setProgress(String.format("%d / %d", position, total));
    }

    private void doRestore() {

        setRestoreState(true);
        List<DataappInfo> listOperate = new ArrayList<DataappInfo>();
        for (int i = 0; i < listBackDataappAll.size(); i++) {
            if (listBackDataappAll.get(i).checked) {
                listOperate.add(listBackDataappAll.get(i));
            }
        }
        ListUtils.setOperateList(listOperate);
        progressBackData.setAppName(getString(R.string.restoring));
        Intent inRestoreService = new Intent(getActivity(), DataRestoreService.class);
        inRestoreService.putExtra("command", FragmentNameConst.FN_RESTORE);
        inRestoreService.putExtra("id", RTConsts.NOTIFY_ID_RESTORE);
        inRestoreService.putExtra("title", R.string.func3p_title);
        inRestoreService.putExtra("desc", R.string.restore_ok);
        inRestoreService.putExtra("proc_id", RTConsts.NOTIFY_PROC_RESTORE);
        inRestoreService.putExtra("proc_title", R.string.func3p_title);
        inRestoreService.putExtra("proc_desc", R.string.restoring_proc);
        getActivity().startService(inRestoreService);
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.barButton1:
                doRestore();
                break;
            case R.id.barButton2:
                ListUtils.setListViewItemSelectedStatus(listBackDataappAll, backDataappAdapter, hSelectData, false);
                break;
            case R.id.chkSelAll:
                boolean selected = barBackData.getCheckBox().isChecked();
                ListUtils.setListViewItemSelectedStatus(listBackDataappAll, backDataappAdapter, hSelectData, selected);
                break;
        }

    }

    @Override
    public boolean onItemLongClick(AdapterView<?> parent, View view,
                                   int position, long id) {
        DataappInfo item = ((DataappInfo) lvBackData.getItemAtPosition(position));
        confirmDeleteBackup(item);
        return false;
    }

    private void doStartLoad() {
        barBackData.setVisibility(View.GONE);
        progressBackData.setAppName(getString(R.string.loading));
        progressBackData.setProgress("");
        progressBackData.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public void onLoadComplete(Loader<List<DataappInfo>> loader, List<DataappInfo> data) {

        listBackDataappAll.clear();
        if (data != null) {
            listBackDataappAll.addAll(data);
        }

        backDataappAdapter.setNewList(listBackDataappAll);
        progressBackData.setVisibility(View.GONE);
        showDataSelectedCount();
        tvEmptyHint.setVisibility(backDataappAdapter.getCount() == 0 ? View.VISIBLE : View.GONE);

    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        if (backDataappAdapter != null) {
            backDataappAdapter.getFilter().filter(newText);
        }
        return false;
    }

    @Override
    public void onStateChange(boolean operating) {
        if (!operating) {
            Intent inRestoreService = new Intent(getActivity(), DataRestoreService.class);
            getActivity().stopService(inRestoreService);
        }
        setRestoreState(operating);

    }

    @Override
    public void onProgress(String name, int position, int total) {
        setRestoreProgress(name, position, total);
    }

    @Override
    public void onMutaxMessage(boolean operating) {
        setOtherProcState(operating);

    }

    @Override
    public void initEvents() {
        lvBackData.setOnItemLongClickListener(this);
        barBackData.getButton1().setOnClickListener(this);
        barBackData.getButton2().setOnClickListener(this);
        barBackData.getCheckBox().setOnClickListener(this);
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
