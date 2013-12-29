package com.rarnu.tools.root.fragment;

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
import android.widget.TextView;
import com.rarnu.command.RootUtils;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.RemainedAdapter;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.RemainedInfo;
import com.rarnu.tools.root.loader.RemainFilesLoader;

import java.util.ArrayList;
import java.util.List;

public class RemainedFilesFragment extends BaseFragment implements Loader.OnLoadCompleteListener<List<RemainedInfo>>, AdapterView.OnItemClickListener {

    MenuItem miRefresh;
    ListView lvApps;
    DataProgressBar progressApps;
    RemainedAdapter adapter;
    List<RemainedInfo> list;
    RemainFilesLoader loader;
    TextView tvEmptyHint;
    private Handler hDelete = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                lvApps.setEnabled(true);
                if (miRefresh != null) {
                    miRefresh.setEnabled(true);
                }
                doStartLoading();
            }
            super.handleMessage(msg);
        }
    };

    @Override
    public int getBarTitle() {
        return R.string.func_remained_files;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_remained_files_with_path;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        lvApps = (ListView) innerView.findViewById(R.id.lvApps);
        progressApps = (DataProgressBar) innerView.findViewById(R.id.progressApps);
        list = new ArrayList<RemainedInfo>();
        adapter = new RemainedAdapter(getActivity(), list);
        lvApps.setAdapter(adapter);
        loader = new RemainFilesLoader(getActivity());
        tvEmptyHint = (TextView) innerView.findViewById(R.id.tvEmptyHint);
    }

    @Override
    public void initEvents() {
        loader.registerListener(0, this);
        lvApps.setOnItemClickListener(this);
    }

    @Override
    public void initLogic() {
        doStartLoading();
    }

    private void doStartLoading() {
        progressApps.setAppName(getString(R.string.loading));
        progressApps.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_remained_files;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        miRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 99, R.string.refresh);
        miRefresh.setIcon(R.drawable.ic_menu_refresh);
        miRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_REFRESH:
                doStartLoading();
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
    public void onLoadComplete(Loader<List<RemainedInfo>> loader, List<RemainedInfo> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
            progressApps.setVisibility(View.GONE);
            tvEmptyHint.setVisibility(list.size() == 0 ? View.VISIBLE : View.GONE);
        }
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        final RemainedInfo item = list.get(position);
        new AlertDialog.Builder(getActivity())
                .setTitle(R.string.hint)
                .setMessage(R.string.remained_files_delete)
                .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        doDeleteFileT(item.packageName);
                    }
                })
                .setNegativeButton(R.string.cancel, null)
                .show();
    }

    private void doDeleteFileT(final String packageName) {
        lvApps.setEnabled(false);
        if (miRefresh != null) {
            miRefresh.setEnabled(false);
        }
        final List<RemainedInfo> listDelete = new ArrayList<RemainedInfo>();
        for (RemainedInfo info : list) {
            if (info.packageName.equals(packageName)) {
                listDelete.add(info);
            }
        }

        new Thread(new Runnable() {
            @Override
            public void run() {
                for (RemainedInfo info : listDelete) {
                    RootUtils.runCommand("rm -r " + info.path, true);
                }
                hDelete.sendEmptyMessage(1);
            }
        }).start();
    }
}
