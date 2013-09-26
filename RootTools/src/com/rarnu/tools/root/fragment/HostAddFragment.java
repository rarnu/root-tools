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
import android.view.View.OnClickListener;
import android.widget.*;
import android.widget.SearchView.OnQueryTextListener;
import com.rarnu.devlib.base.BasePopupFragment;
import com.rarnu.devlib.component.DataBar;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.HostsAdapter;
import com.rarnu.tools.root.common.HostRecordInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.loader.HostsSearchLoader;

import java.util.ArrayList;
import java.util.List;

public class HostAddFragment extends BasePopupFragment implements OnClickListener, OnLoadCompleteListener<List<HostRecordInfo>>, OnQueryTextListener {

    ListView lvAddHosts;
    DataBar barAddHosts;
    DataProgressBar progressSearchHosts;
    Button btnCom, btnOrg, btnNet, btnEdu, btnInfo, btnBiz, btnCn, btnUs, btnJp, btnHk, btnTw;
    SearchView sv;
    HostsAdapter adapter = null;
    List<HostRecordInfo> list = new ArrayList<HostRecordInfo>();
    HostsSearchLoader loader = null;
    MenuItem itemSearch;
    MenuItem itemAdd;
    Handler hSelectHost = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                showHostSelectedCount();
            }
            super.handleMessage(msg);
        }
    };

    @Override
    public int getBarTitle() {
        return R.string.host_add;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.host_add;
    }

    @Override
    public void initComponents() {

        lvAddHosts = (ListView) innerView.findViewById(R.id.lvAddHosts);
        barAddHosts = (DataBar) innerView.findViewById(R.id.barAddHosts);
        progressSearchHosts = (DataProgressBar) innerView.findViewById(R.id.progressSearchHosts);
        btnCom = (Button) innerView.findViewById(R.id.btnCom);
        btnOrg = (Button) innerView.findViewById(R.id.btnOrg);
        btnNet = (Button) innerView.findViewById(R.id.btnNet);
        btnEdu = (Button) innerView.findViewById(R.id.btnEdu);
        btnInfo = (Button) innerView.findViewById(R.id.btnInfo);
        btnBiz = (Button) innerView.findViewById(R.id.btnBiz);
        btnCn = (Button) innerView.findViewById(R.id.btnCn);
        btnUs = (Button) innerView.findViewById(R.id.btnUs);
        btnJp = (Button) innerView.findViewById(R.id.btnJp);
        btnHk = (Button) innerView.findViewById(R.id.btnHk);
        btnTw = (Button) innerView.findViewById(R.id.btnTw);
        adapter = new HostsAdapter(getActivity(), list, hSelectHost, false, true);
        if (lvAddHosts != null) {
            lvAddHosts.setAdapter(adapter);
        }
        loader = new HostsSearchLoader(getActivity());
    }

    @Override
    public void initLogic() {
        adapter.notifyDataSetChanged();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_host_add;
    }

    @Override
    public void initMenu(Menu menu) {
        itemSearch = menu.add(0, MenuItemIds.MENU_SEARCH, 98, R.string.search);
        itemSearch.setIcon(android.R.drawable.ic_menu_search);
        itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        sv = new SearchView(getActivity());
        sv.setIconifiedByDefault(true);
        sv.setOnQueryTextListener(this);
        itemSearch.setActionView(sv);

        itemAdd = menu.add(0, MenuItemIds.MENU_ADD, 99, R.string.add);
        itemAdd.setIcon(android.R.drawable.ic_menu_add);
        itemAdd.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_ADD:
                returnAddHosts();
                break;
        }
        return true;
    }

    private void doSearch(final String domain) {

        barAddHosts.setVisibility(View.GONE);
        progressSearchHosts.setAppName(getString(R.string.searching));
        progressSearchHosts.setVisibility(View.VISIBLE);
        loader.setDomain(domain);
        loader.startLoading();
    }

    private void showHostSelectedCount() {
        try {
            int count = getHostSelectedCount(list);
            String cap = String.format(getResources().getString(R.string.btn_delete), count);
            barAddHosts.setButton1Text(cap);
            barAddHosts.setVisibility(count == 0 ? View.GONE : View.VISIBLE);
        } catch (Exception e) {

        }
    }

    private int getHostSelectedCount(List<HostRecordInfo> list) {
        int count = 0;
        if (list != null) {
            for (int i = 0; i < list.size(); i++) {
                if (list.get(i).checked) {
                    count++;
                }
            }
        }
        return count;
    }

    private void setHostItemSelectedStatus(List<HostRecordInfo> list, BaseAdapter adapter, Handler h, boolean selected) {
        for (int i = 0; i < list.size(); i++) {
            list.get(i).checked = selected;
        }
        adapter.notifyDataSetChanged();
        h.sendEmptyMessage(1);
    }

    private void deleteSelectedItemsFromList() {
        int count = list.size();
        for (int i = count - 1; i >= 0; i--) {
            if (list.get(i).checked) {
                list.remove(i);
            }
        }
        adapter.notifyDataSetChanged();
        showHostSelectedCount();
    }

    private void returnAddHosts() {
        if (list == null || list.size() == 0) {
            Toast.makeText(getActivity(), R.string.no_host_for_add, Toast.LENGTH_LONG).show();
            return;
        }
        String host = "";
        String[] strHosts = new String[list.size()];
        int cnt = 0;
        for (HostRecordInfo info : list) {
            host = String.format("%s\t%s", info.ip, info.domain);
            strHosts[cnt] = new String(host);
            cnt++;
        }

        Intent inReturn = new Intent();
        inReturn.putExtra("hosts", strHosts);
        getActivity().setResult(Activity.RESULT_OK, inReturn);
        getActivity().finish();
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.barButton1:
                deleteSelectedItemsFromList();
                return;
            case R.id.barButton2:
                setHostItemSelectedStatus(list, adapter, hSelectHost, false);
                return;
            case R.id.chkSelAll:
                boolean selected = barAddHosts.getCheckBox().isChecked();
                setHostItemSelectedStatus(list, adapter, hSelectHost, selected);
                break;
            default:
                Button btnQuery = (Button) v;
                sv.setQuery(sv.getQuery() + btnQuery.getText().toString(), false);
                break;
        }

    }

    @Override
    public void onLoadComplete(Loader<List<HostRecordInfo>> loader, List<HostRecordInfo> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        adapter.setNewList(list);
        progressSearchHosts.setVisibility(View.GONE);
        showHostSelectedCount();

    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        String domain = query;
        if (domain == null || domain.equals("")) {
            Toast.makeText(getActivity(), R.string.domain_name_empty, Toast.LENGTH_LONG).show();
            return true;
        }
        doSearch(domain);
        sv.setQuery("", false);
        sv.setIconified(true);
        return true;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        return false;
    }

    @Override
    public void initEvents() {
        barAddHosts.getButton1().setOnClickListener(this);
        barAddHosts.getButton2().setOnClickListener(this);
        btnCom.setOnClickListener(this);
        btnOrg.setOnClickListener(this);
        btnNet.setOnClickListener(this);
        btnEdu.setOnClickListener(this);
        btnInfo.setOnClickListener(this);
        btnBiz.setOnClickListener(this);
        btnCn.setOnClickListener(this);
        btnUs.setOnClickListener(this);
        btnJp.setOnClickListener(this);
        btnHk.setOnClickListener(this);
        btnTw.setOnClickListener(this);
        barAddHosts.getCheckBox().setOnClickListener(this);
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
