package com.rarnu.tools.root.fragment;

import android.content.Loader;
import android.graphics.Color;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.*;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.AutobootAdapter;
import com.rarnu.tools.root.common.AutobootInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.loader.AutobootLoader;
import com.rarnu.tools.root.utils.AutobootUtils;
import com.rarnu.utils.ImageUtils;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;

import java.util.ArrayList;
import java.util.List;

public class AutoBootFragment extends BaseFragment implements Loader.OnLoadCompleteListener<List<AutobootInfo>>, AdapterView.OnItemLongClickListener, SearchView.OnQueryTextListener {

    ListView lvComp;
    DataProgressBar progressComp;
    SearchView sv;
    AutobootLoader loader;
    TextView tvOperateHint;
    AutobootAdapter adapter;
    List<AutobootInfo> list;
    MenuItem miRefresh;

    public AutoBootFragment() {
        super();
        tabTitle = ResourceUtils.getString(R.string.func_auto_boot);
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
        progressComp = (DataProgressBar) innerView.findViewById(R.id.progressComp);
        lvComp = (ListView) innerView.findViewById(R.id.lvComp);
        sv = (SearchView) innerView.findViewById(R.id.sv);
        tvOperateHint = (TextView) innerView.findViewById(R.id.tvOperateHint);
        loader = new AutobootLoader(getActivity());
        list = new ArrayList<AutobootInfo>();
        adapter = new AutobootAdapter(getActivity(), list);
        lvComp.setAdapter(adapter);
    }

    @Override
    public void initEvents() {
        loader.registerListener(0, this);
        lvComp.setOnItemLongClickListener(this);
        sv.setOnQueryTextListener(this);
    }

    @Override
    public void initLogic() {
        doStartLoading();
    }

    private void doStartLoading() {
        tvOperateHint.setVisibility(View.GONE);
        progressComp.setAppName(getString(R.string.loading));
        progressComp.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_autoboot;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        miRefresh = menu.add(0, MenuItemIds.MENU_REFRESH, 99, R.string.refresh);
        miRefresh.setIcon(ImageUtils.loadActionBarIcon(getActivity(), R.drawable.ic_menu_refresh));
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
    public void onLoadComplete(Loader<List<AutobootInfo>> loader, List<AutobootInfo> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
            progressComp.setVisibility(View.GONE);
            tvOperateHint.setVisibility(View.VISIBLE);
        }
    }

    @Override
    public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
        AutobootInfo item = (AutobootInfo) lvComp.getItemAtPosition(position);
        doChangeStatusT((TextView) view.findViewById(R.id.tvEnabled), item);
        return true;
    }

    private void doChangeStatusT(final TextView tv, final AutobootInfo item) {
        final boolean enabled = item.enabled;
        item.processing = true;
        tv.setTextColor(Color.YELLOW);
        tv.setText(R.string.loading);
        final Handler hToggle = new Handler() {
            @Override
            public void handleMessage(Message msg) {
                if (msg.what == 1) {
                    item.processing = false;
                    if (getActivity() != null) {
                        if (msg.arg1 != 0) {
                            item.enabled = !enabled;
                        } else {
                            Toast.makeText(getActivity(), R.string.operation_failed, Toast.LENGTH_LONG).show();
                        }
                        tv.setText(item.enabled ? R.string.package_enabled : R.string.package_disabled);
                        tv.setTextColor(item.enabled ? Color.GREEN : Color.RED);
                    }
                }
                super.handleMessage(msg);
            }
        };

        new Thread(new Runnable() {
            @Override
            public void run() {
                boolean ret = AutobootUtils.switchAutoboot(getActivity(), item, UIUtils.getDM(), !item.enabled);
                Message msg = new Message();
                msg.what = 1;
                msg.arg1 = ret ? 1 : 0;
                hToggle.sendMessage(msg);
            }
        }).start();

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
