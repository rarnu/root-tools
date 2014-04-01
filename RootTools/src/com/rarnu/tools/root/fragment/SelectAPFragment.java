package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.content.Intent;
import android.net.wifi.ScanResult;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.APAdapter;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.utils.WifiUtils;

import java.util.ArrayList;
import java.util.List;

public class SelectAPFragment extends BaseFragment implements AdapterView.OnItemLongClickListener, View.OnClickListener {

    WifiUtils wifi;
    ListView lvAp;
    List<ScanResult> list;
    APAdapter adapter;
    TextView tvRefreshAp;
    MenuItem miRefresh;

    private Handler hScan = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                loadScannedAP();
            }
            super.handleMessage(msg);
        }
    };

    private void loadScannedAP() {
        list.clear();
        List<ScanResult> tmp = wifi.getWifiList();
        for (ScanResult sr : tmp) {
            if (sr.SSID.startsWith("RootTools-")) {
                list.add(sr);
            }
        }
        adapter.setNewList(list);
        tvRefreshAp.setVisibility(list.size() == 0 ? View.VISIBLE : View.GONE);
    }

    @Override
    public int getBarTitle() {
        return R.string.ft_select_ap;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.ft_select_ap;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        wifi = new WifiUtils(getActivity());
        lvAp = (ListView) innerView.findViewById(R.id.lvAp);
        list = new ArrayList<ScanResult>();
        adapter = new APAdapter(getActivity(), list);
        lvAp.setAdapter(adapter);
        tvRefreshAp = (TextView) innerView.findViewById(R.id.tvRefreshAp);
    }

    @Override
    public void initEvents() {
        lvAp.setOnItemLongClickListener(this);
        tvRefreshAp.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        doScanAPT();
    }

    private void doScanAPT() {
        new Thread(new Runnable() {
            @Override
            public void run() {
                wifi.openWifi();
                wifi.startScan();
                try {
                    Thread.sleep(500);
                } catch (Exception e) {

                }
                hScan.sendEmptyMessage(1);
            }
        }).start();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_select_ap;
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
                doScanAPT();
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
    public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
        final ScanResult item = (ScanResult) lvAp.getItemAtPosition(position);
        Intent inRet = new Intent();
        inRet.putExtra("filePath", getActivity().getIntent().getStringExtra("filePath"));
        inRet.putExtra("id", item.SSID);
        getActivity().setResult(Activity.RESULT_OK, inRet);
        getActivity().finish();
        return true;
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.tvRefreshAp:
                doScanAPT();
                break;
        }
    }
}
