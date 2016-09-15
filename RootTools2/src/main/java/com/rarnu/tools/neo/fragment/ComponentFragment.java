package com.rarnu.tools.neo.fragment;

import android.content.Intent;
import android.content.Loader;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.SearchView;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.activity.ComponentDetailActivity;
import com.rarnu.tools.neo.adapter.AppAdapter;
import com.rarnu.tools.neo.base.BaseFragment;
import com.rarnu.tools.neo.comp.LoadingView;
import com.rarnu.tools.neo.data.AppInfo;
import com.rarnu.tools.neo.loader.AllAppLoader;

import java.util.ArrayList;
import java.util.List;

public class ComponentFragment extends BaseFragment implements AdapterView.OnItemClickListener, SearchView.OnQueryTextListener {

    private List<AppInfo> list = null;
    private AppAdapter adapter = null;
    private ListView lvApp = null;
    private AllAppLoader loader = null;
    private SearchView sv = null;
    private MenuItem miSearch = null;
    private LoadingView loading = null;

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        AppInfo item = adapter.getFiltedItem(position);
        Intent inDetail = new Intent(getContext(), ComponentDetailActivity.class);
        inDetail.putExtra("pkg", item.packageName);
        inDetail.putExtra("versionCode", item.versionCode);
        inDetail.putExtra("name", item.name);
        startActivity(inDetail);

    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        adapter.filter(newText);
        return true;
    }

    @Override
    public int getBarTitle() {
        return R.string.component_name;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        lvApp = (ListView) innerView.findViewById(R.id.lvApp);
        loading = (LoadingView) innerView.findViewById(R.id.loading);
        list = new ArrayList<>();
        adapter = new AppAdapter(getContext(), list);
        adapter.setShowSwitch(false);
        lvApp.setAdapter(adapter);
        loader = new AllAppLoader(getContext());
    }

    @Override
    public void initEvents() {
        lvApp.setOnItemClickListener(this);
        loader.registerListener(0, new Loader.OnLoadCompleteListener<List<AppInfo>>() {
            @Override
            public void onLoadComplete(Loader<List<AppInfo>> loader, List<AppInfo> data) {
                list.clear();
                if (data != null) {
                    list.addAll(data);
                }
                adapter.setNewList(list);
                loading.setVisibility(View.GONE);
            }
        });
    }

    @Override
    public void initLogic() {
        loading.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_component;
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
}
