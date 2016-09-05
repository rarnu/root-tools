package com.rarnu.tools.neo.fragment;

import android.content.Loader;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.SearchView;
import android.widget.Toast;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.adapter.BannedAdapter;
import com.rarnu.tools.neo.base.BaseFragment;
import com.rarnu.tools.neo.comp.LoadingView;
import com.rarnu.tools.neo.data.BanStartInfo;
import com.rarnu.tools.neo.loader.BannedLoader;
import com.rarnu.tools.neo.xposed.XpStatus;

import java.util.ArrayList;
import java.util.List;

public class BanStartFragment extends BaseFragment implements AdapterView.OnItemClickListener, SearchView.OnQueryTextListener {

    private ListView lvApp = null;
    private LoadingView loading = null;
    private List<BanStartInfo> list = null;
    private BannedAdapter adapter = null;
    private BannedLoader loader = null;

    private MenuItem miSearch = null;
    private SearchView sv = null;
    private SharedPreferences pref = null;
    private SharedPreferences.Editor editor = null;

    @Override
    public int getBarTitle() {
        return R.string.banstart_name;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        pref = getContext().getSharedPreferences(XpStatus.PREF, 1);
        editor = pref.edit();
        lvApp = (ListView) innerView.findViewById(R.id.lvApp);
        loading = (LoadingView) innerView.findViewById(R.id.loading);
        list = new ArrayList<>();
        adapter = new BannedAdapter(getContext(), list);
        lvApp.setAdapter(adapter);
        loader = new BannedLoader(getContext());
    }

    @Override
    public void initEvents() {
        lvApp.setOnItemClickListener(this);
        loader.registerListener(0, new Loader.OnLoadCompleteListener<List<BanStartInfo>>() {
            @Override
            public void onLoadComplete(Loader<List<BanStartInfo>> loader, List<BanStartInfo> data) {
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
        return R.layout.fragment_banstart;
    }

    @Override
    public String getMainActivityName() {
        return null;
    }

    @Override
    public void initMenu(Menu menu) {
        menu.clear();
        miSearch = menu.add(0, 1, 1, R.string.ab_search);
        miSearch.setIcon(android.R.drawable.ic_menu_search);
        miSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
        sv = new SearchView(getContext());
        sv.setOnQueryTextListener(this);
        miSearch.setActionView(sv);
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
        BanStartInfo item = adapter.getFiltedItem(position);
        Toast.makeText(getContext(), item.packageName, Toast.LENGTH_SHORT).show();
        item.isBanned = !item.isBanned;
        editor.putBoolean("banned_" + item.packageName, item.isBanned).apply();
        adapter.notifyDataSetChanged();
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
}
