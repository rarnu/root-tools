package com.rarnu.tools.root.fragment;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.GridView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.HostsAdapter;
import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.loader.HostsLoader;
import com.rarnu.tools.root.utils.DIPairUtils;
import com.rarnu.utils.NetworkUtils;

import java.util.ArrayList;
import java.util.List;

public class HostDeprecatedFragment extends BasePopupFragment implements OnLoadCompleteListener<List<HostRecordInfo>> {

    final Handler hScanHosts = new Handler() {
        @Override
        public void handleMessage(Message msg) {

            if (msg.what == 1) {
                progressDeprecated.setVisibility(View.GONE);
                itemScan.setEnabled(true);
                adapter.notifyDataSetChanged();

                boolean ret = DIPairUtils.saveHosts(lstDeprecated);
                if (ret) {
                    Toast.makeText(getActivity(), R.string.save_hosts_succ, Toast.LENGTH_LONG).show();
                    getActivity().finish();
                } else {
                    Toast.makeText(getActivity(), R.string.save_hosts_error, Toast.LENGTH_LONG).show();
                }
            } else if (msg.what == 2) {
                progressDeprecated.setProgress((String) msg.obj);
            }
            super.handleMessage(msg);
        }
    };
    ListView lvDeprecatedHosts;
    GridView gvDeprecatedHosts;
    DataProgressBar progressDeprecated;
    TextView tvTooBigHint;
    List<HostRecordInfo> lstDeprecated = new ArrayList<HostRecordInfo>();
    HostsAdapter adapter = null;
    HostsLoader loader = null;
    MenuItem itemScan;
    private Handler hSelectHost = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            super.handleMessage(msg);
        }

    };

    @Override
    public int getBarTitle() {
        return R.string.clean_deprecated_hosts;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.clean_deprecated_hosts;
    }

    @Override
    public void initComponents() {
        lvDeprecatedHosts = (ListView) innerView.findViewById(R.id.lvDeprecatedHosts);
        gvDeprecatedHosts = (GridView) innerView.findViewById(R.id.gvDeprecatedHosts);
        progressDeprecated = (DataProgressBar) innerView.findViewById(R.id.progressDeprecated);
        tvTooBigHint = (TextView) innerView.findViewById(R.id.tvTooBigHint);
        adapter = new HostsAdapter(getActivity(), lstDeprecated, hSelectHost, false, false);
        if (lvDeprecatedHosts != null) {
            lvDeprecatedHosts.setAdapter(adapter);
        }
        if (gvDeprecatedHosts != null) {
            gvDeprecatedHosts.setAdapter(adapter);
        }
        loader = new HostsLoader(getActivity());
    }

    @Override
    public void initLogic() {
        doStartLoad();
    }

    private void doStartLoad() {
        progressDeprecated.setAppName(getString(R.string.loading));
        progressDeprecated.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_host_deprecated;
    }

    @Override
    public void initMenu(Menu menu) {
        itemScan = menu.add(0, MenuItemIds.MENU_SCAN, 99, R.string.scan);
        itemScan.setIcon(android.R.drawable.ic_menu_manage);
        itemScan.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        itemScan.setEnabled(lstDeprecated.size() != 0);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_SCAN:
                scanDeprecatedHostsT();
                break;
        }
        return true;
    }

    private void scanDeprecatedHostsT() {

        progressDeprecated.setAppName(getString(R.string.testing));
        progressDeprecated.setVisibility(View.VISIBLE);
        itemScan.setEnabled(false);
        new Thread(new Runnable() {

            @Override
            public void run() {
                String ping = "";
                int count = lstDeprecated.size();
                for (int i = count - 1; i >= 0; i--) {
                    Message msg = new Message();
                    msg.what = 2;
                    msg.obj = lstDeprecated.get(i).ip;
                    hScanHosts.sendMessage(msg);
                    ping = NetworkUtils.ping(lstDeprecated.get(i).ip);
                    if (ping.equals("") || ping.equals("timeout")) {
                        lstDeprecated.remove(i);
                    }
                }
                hScanHosts.sendEmptyMessage(1);
            }
        }).start();
    }

    @Override
    public void onLoadComplete(Loader<List<HostRecordInfo>> loader, List<HostRecordInfo> data) {
        lstDeprecated.clear();
        if (data != null) {
            lstDeprecated.addAll(data);
        }
        adapter.setNewList(lstDeprecated);
        progressDeprecated.setVisibility(View.GONE);
        tvTooBigHint.setVisibility(data == null ? View.VISIBLE : View.GONE);
        if (itemScan != null) {
            itemScan.setEnabled(data != null);
        }
    }

    @Override
    public void initEvents() {
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
