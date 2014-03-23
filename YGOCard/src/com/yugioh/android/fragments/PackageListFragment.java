package com.yugioh.android.fragments;

import android.content.Intent;
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
import android.widget.Toast;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.yugioh.android.PackageCardsActivity;
import com.yugioh.android.R;
import com.yugioh.android.adapter.PackageAdapter;
import com.yugioh.android.classes.CardItems;
import com.yugioh.android.classes.PackageItem;
import com.yugioh.android.common.MenuIds;
import com.yugioh.android.loader.PackageLoader;
import com.yugioh.android.utils.MiscUtils;

import java.util.ArrayList;
import java.util.List;


public class PackageListFragment extends BaseFragment implements AdapterView.OnItemClickListener, Loader.OnLoadCompleteListener<List<PackageItem>> {

    ListView lvPackage;
    TextView tvLoading;
    PackageLoader loader;
    PackageAdapter adapter;
    TextView tvListNoPackage;
    List<PackageItem> list;
    MenuItem itemRefresh;
    private Handler hPack = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                lvPackage.setEnabled(true);
                tvLoading.setVisibility(View.GONE);
                Bundle bn = new Bundle();
                CardItems items = (CardItems) msg.obj;
                if (items != null) {
                    bn.putIntArray("ids", items.cardIds);
                    bn.putString("pack", items.packageName);
                    bn.putString("id", items.id);
                    Intent inCards = new Intent(getActivity(), PackageCardsActivity.class);
                    inCards.putExtras(bn);
                    startActivity(inCards);
                } else {
                    Toast.makeText(getActivity(), R.string.package_cannot_load, Toast.LENGTH_LONG).show();
                }
            }
            super.handleMessage(msg);
        }
    };

    public PackageListFragment() {
        super();
        tabTitle = ResourceUtils.getString(R.string.package_list);
    }

    @Override
    public int getBarTitle() {
        return R.string.lm_package;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.lm_package;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        lvPackage = (ListView) innerView.findViewById(R.id.lvPackage);
        tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
        tvListNoPackage = (TextView) innerView.findViewById(R.id.tvListNoPackage);
        loader = new PackageLoader(getActivity());
        list = new ArrayList<PackageItem>();
        adapter = new PackageAdapter(getActivity(), list);
        lvPackage.setAdapter(adapter);
    }

    @Override
    public void initEvents() {
        lvPackage.setOnItemClickListener(this);
        loader.registerListener(0, this);
    }

    @Override
    public void initLogic() {
        tvListNoPackage.setText(R.string.package_nocard_search);
        tvLoading.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_package_list;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu menu) {
        itemRefresh = menu.add(0, MenuIds.MENUID_REFRESH, 99, R.string.refresh);
        itemRefresh.setIcon(android.R.drawable.ic_menu_revert);
        itemRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuIds.MENUID_REFRESH:
                tvLoading.setVisibility(View.VISIBLE);
                loader.setRefresh(true);
                loader.startLoading();
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
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        tvLoading.setVisibility(View.VISIBLE);
        lvPackage.setEnabled(false);
        PackageItem item = list.get(position);
        MiscUtils.loadCardsDataT(0, item.id, hPack, false);
    }

    @Override
    public void onLoadComplete(Loader<List<PackageItem>> loader, List<PackageItem> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
            tvLoading.setVisibility(View.GONE);
            tvListNoPackage.setVisibility(list.size() == 0 ? View.VISIBLE : View.GONE);
            tvListNoPackage.setText(R.string.package_list_not_exist);
        }
    }
}
