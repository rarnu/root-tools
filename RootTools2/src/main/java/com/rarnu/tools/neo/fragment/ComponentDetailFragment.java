package com.rarnu.tools.neo.fragment;

import android.app.AlertDialog;
import android.content.Loader;
import android.content.pm.ApplicationInfo;
import android.graphics.Color;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.*;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.adapter.CompDetailAdapter;
import com.rarnu.tools.neo.api.API;
import com.rarnu.tools.neo.api.NativeAPI;
import com.rarnu.tools.neo.base.BaseFragment;
import com.rarnu.tools.neo.comp.LoadingView;
import com.rarnu.tools.neo.data.CompInfo;
import com.rarnu.tools.neo.data.Onekey;
import com.rarnu.tools.neo.loader.ComponentLoader;
import com.rarnu.tools.neo.utils.ComponentUtils;
import com.rarnu.tools.neo.utils.PackageParserUtils;

import java.util.ArrayList;
import java.util.List;

public class ComponentDetailFragment extends BaseFragment implements View.OnClickListener, SearchView.OnQueryTextListener, AdapterView.OnItemClickListener, AdapterView.OnItemLongClickListener {

    private TextView btnActivity = null;
    private TextView btnService = null;
    private TextView btnReceiver = null;
    private TextView btnProvider = null;
    private ListView lvComponent = null;
    private LoadingView loading = null;
    private RelativeLayout layProfile = null;
    private TextView btnProfile = null;

    private String pkgName = null;
    private int versionCode = 0;
    private int focusItem = -1;
    private ComponentLoader loader = null;
    private List<CompInfo> list = null;
    private CompDetailAdapter adapter = null;

    private MenuItem miSearch = null;
    private SearchView sv = null;
    private String filterText = "";

    private TextView tvPkgName = null;
    private TextView tvVer = null;
    private Button btnDownloadProfile = null;
    private Button btnUploadProfile = null;

    @Override
    public void onClick(View v) {
        lvComponent.setVisibility(View.VISIBLE);
        layProfile.setVisibility(View.GONE);
        unfocusButtons();
        switch (v.getId()) {
            case R.id.btnActivity:
                if (focusItem != 0) {
                    doLoadData(0);
                    focusButton(btnActivity);
                }
                break;
            case R.id.btnService:
                if (focusItem != 1) {
                    doLoadData(1);
                    focusButton(btnService);
                }
                break;
            case R.id.btnReceiver:
                if (focusItem != 2) {
                    doLoadData(2);
                    focusButton(btnReceiver);
                }
                break;
            case R.id.btnProvider:
                if (focusItem != 3) {
                    doLoadData(3);
                    focusButton(btnProvider);
                }
                break;
            case R.id.btnProfile:
                focusItem = 4;
                lvComponent.setVisibility(View.GONE);
                layProfile.setVisibility(View.VISIBLE);
                focusButton(btnProfile);
                break;
        }
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        CompInfo item = adapter.getFiltedItem(position);
        threadChangeComponentFreeze(item);
    }

    @Override
    public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
        // long click for intent filter detail
        CompInfo item = adapter.getFiltedItem(position);
        List<String> msg = item.getIntents();
        String msgStr = "";
        if (msg == null || msg.size() == 0) {
            msgStr = getString(R.string.alert_no_intent);
        } else {
            for (String s : msg) {
                msgStr += s + "\n";
            }
        }
        new AlertDialog.Builder(getContext())
                .setTitle(R.string.alert_hint)
                .setMessage(msgStr)
                .setPositiveButton(R.string.alert_ok, null)
                .show();
        return true;
    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        filterText = newText;
        adapter.filter(newText);
        return true;
    }

    @Override
    public int getBarTitle() {
        return 0;
    }

    @Override
    public String getCustomTitle() {
        return getActivity().getIntent().getStringExtra("name");
    }

    @Override
    public void initComponents() {
        btnActivity = (TextView) innerView.findViewById(R.id.btnActivity);
        btnService = (TextView) innerView.findViewById(R.id.btnService);
        btnReceiver = (TextView) innerView.findViewById(R.id.btnReceiver);
        btnProvider = (TextView) innerView.findViewById(R.id.btnProvider);
        btnProfile = (TextView) innerView.findViewById(R.id.btnProfile);
        lvComponent = (ListView) innerView.findViewById(R.id.lvComponent);
        layProfile = (RelativeLayout) innerView.findViewById(R.id.layProfile);
        tvPkgName = (TextView) innerView.findViewById(R.id.tvPkgName);
        tvVer = (TextView) innerView.findViewById(R.id.tvVer);
        btnDownloadProfile = (Button) innerView.findViewById(R.id.btnDownloadProfile);
        btnUploadProfile = (Button) innerView.findViewById(R.id.btnUploadProfile);

        loading = (LoadingView) innerView.findViewById(R.id.loading);
        list = new ArrayList<>();
        adapter = new CompDetailAdapter(getContext(), list);
        lvComponent.setAdapter(adapter);
        loader = new ComponentLoader(getContext());
    }

    @Override
    public void initEvents() {
        lvComponent.setOnItemClickListener(this);
        lvComponent.setOnItemLongClickListener(this);
        btnActivity.setOnClickListener(this);
        btnService.setOnClickListener(this);
        btnReceiver.setOnClickListener(this);
        btnProvider.setOnClickListener(this);
        btnProfile.setOnClickListener(this);
        btnDownloadProfile.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                threadGetOnekeyAndApply();
            }
        });
        btnUploadProfile.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                threadPutOnekeyConfig();
            }
        });

        loader.registerListener(0, new Loader.OnLoadCompleteListener<List<CompInfo>>() {
            @Override
            public void onLoadComplete(Loader<List<CompInfo>> loader, List<CompInfo> data) {
                if (data != null) {
                    list.addAll(data);
                    adapter.setNewList(list);
                    if (!filterText.equals("")) {
                        adapter.filter(filterText);
                    }
                }
                loading.setVisibility(View.GONE);
            }
        });
    }

    @Override
    public void initLogic() {
        pkgName = getActivity().getIntent().getStringExtra("pkg");
        versionCode = getActivity().getIntent().getIntExtra("versionCode", 0);
        tvPkgName.setText(getString(R.string.view_profile_pkg, pkgName));
        tvVer.setText(getString(R.string.view_profile_ver, versionCode));
        onClick(btnActivity);
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_component_detail;
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

    private void unfocusButtons() {
        btnActivity.setBackground(getResources().getDrawable(R.drawable.button_normal, getContext().getTheme()));
        btnService.setBackground(getResources().getDrawable(R.drawable.button_normal, getContext().getTheme()));
        btnReceiver.setBackground(getResources().getDrawable(R.drawable.button_normal, getContext().getTheme()));
        btnProvider.setBackground(getResources().getDrawable(R.drawable.button_normal, getContext().getTheme()));
        btnProfile.setBackground(getResources().getDrawable(R.drawable.button_normal, getContext().getTheme()));
        btnActivity.setTextColor(Color.BLACK);
        btnService.setTextColor(Color.BLACK);
        btnReceiver.setTextColor(Color.BLACK);
        btnProvider.setTextColor(Color.BLACK);
        btnProfile.setTextColor(Color.BLACK);
    }

    private void focusButton(TextView btn) {
        btn.setBackground(getResources().getDrawable(R.drawable.button_focus, getContext().getTheme()));
        btn.setTextColor(Color.WHITE);
    }

    private void doLoadData(int type) {
        loading.setVisibility(View.VISIBLE);
        focusItem = type;
        list.clear();
        adapter.notifyDataSetChanged();
        loader.startLoading(pkgName, type);
    }

    private Handler hFreeze = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 0) {
                Toast.makeText(getContext(), R.string.toast_component_fail, Toast.LENGTH_SHORT).show();
            }
            adapter.notifyDataSetChanged();
            lvComponent.setEnabled(true);
            super.handleMessage(msg);
        }
    };

    private void threadChangeComponentFreeze(final CompInfo item) {
        lvComponent.setEnabled(false);
        new Thread(new Runnable() {
            @Override
            public void run() {
                boolean newStat = !item.enabled;
                boolean ret = NativeAPI.freezeComponent(pkgName, item.component.className, !newStat);
                if (ret) {
                    item.enabled = newStat;
                }
                Message msg = new Message();
                msg.what = ret ? 1 : 0;
                msg.obj = item;
                hFreeze.sendMessage(msg);
            }
        }).start();
    }

    private Handler hOnekey = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            showApplyProfileAlert(msg.what == 1);
            btnDownloadProfile.setEnabled(true);
            btnUploadProfile.setEnabled(true);
            loading.setVisibility(View.GONE);
            super.handleMessage(msg);
        }
    };

    private void showApplyProfileAlert(boolean hasProfile) {
        new AlertDialog.Builder(getContext())
                .setTitle(R.string.alert_hint)
                .setMessage(hasProfile ? R.string.alert_apply_component_config : R.string.alert_no_component_config)
                .setPositiveButton(R.string.alert_ok, null)
                .show();
    }

    private void threadGetOnekeyAndApply() {
        btnDownloadProfile.setEnabled(false);
        btnUploadProfile.setEnabled(false);
        loading.setVisibility(View.VISIBLE);
        new Thread(new Runnable() {
            @Override
            public void run() {
                boolean hasProfile = false;
                Onekey ok = API.getOnekey(pkgName, versionCode);
                if (ok != null) {
                    if (ok.disabledComponents != null && ok.disabledComponents.length != 0) {
                        hasProfile = true;
                    }
                    if (hasProfile) {
                        NativeAPI.freezeComponents(pkgName, ok.disabledComponents, true);
                    }
                }
                hOnekey.sendEmptyMessage(hasProfile ? 1 : 0);
            }
        }).start();
    }

    private Handler hPutOnekey = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            // handle put onekey
            showUploadProfileAlert(msg.what == 1);
            btnDownloadProfile.setEnabled(true);
            btnUploadProfile.setEnabled(true);
            loading.setVisibility(View.GONE);
            super.handleMessage(msg);
        }
    };

    private void showUploadProfileAlert(boolean succ) {
        new AlertDialog.Builder(getContext())
                .setTitle(R.string.alert_hint)
                .setMessage(succ ? R.string.alert_upload_ok : R.string.alert_upload_failed)
                .setPositiveButton(R.string.alert_ok, null)
                .show();
    }

    private void threadPutOnekeyConfig() {
        // thread put onekey config
        btnDownloadProfile.setEnabled(false);
        btnUploadProfile.setEnabled(false);
        loading.setVisibility(View.VISIBLE);
        new Thread(new Runnable() {
            @Override
            public void run() {
                boolean ret = false;
                try {
                    ApplicationInfo info = getContext().getPackageManager().getApplicationInfo(pkgName, 0);
                    PackageParserUtils ppu = new PackageParserUtils();
                    Object obj = ppu.parsePackage(info.publicSourceDir, 0);
                    List<CompInfo> lstActivity = ComponentUtils.getActivityList(getContext(), obj);
                    List<CompInfo> lstService = ComponentUtils.getServiceList(getContext(), obj);
                    List<CompInfo> lstReceiver = ComponentUtils.getReceiverList(getContext(), obj);
                    List<CompInfo> lstProvider = ComponentUtils.getProviderList(getContext(), obj);
                    List<String> lstDisabled = new ArrayList<>();
                    for (CompInfo ci: lstActivity) {
                        if (!ci.enabled) {
                            lstDisabled.add(ci.component.className);
                        }
                    }
                    for (CompInfo ci: lstService) {
                        if (!ci.enabled) {
                            lstDisabled.add(ci.component.className);
                        }
                    }
                    for (CompInfo ci: lstReceiver) {
                        if (!ci.enabled) {
                            lstDisabled.add(ci.component.className);
                        }
                    }
                    for (CompInfo ci: lstProvider) {
                        if (!ci.enabled) {
                            lstDisabled.add(ci.component.className);
                        }
                    }
                    if (lstDisabled.size() != 0) {
                        ret = API.uploadOnekey(pkgName, versionCode, lstDisabled);
                    }
                } catch (Exception e) {

                }
                hPutOnekey.sendEmptyMessage(ret ? 1 : 0);
            }
        }).start();
    }
}
