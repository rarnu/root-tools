package com.rarnu.tools.neo.fragment;

import android.app.Activity;
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
import android.widget.SearchView;
import android.widget.Toast;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.activity.BuildPropEditActivity;
import com.rarnu.tools.neo.adapter.BuildPropAdapter;
import com.rarnu.tools.neo.base.BaseFragment;
import com.rarnu.tools.neo.comp.LoadingView;
import com.rarnu.tools.neo.data.BuildPropInfo;
import com.rarnu.tools.neo.loader.BuildPropLoader;
import com.rarnu.tools.neo.utils.BuildPropUtils;

import java.util.ArrayList;
import java.util.List;

public class FakeDeviceFragment extends BaseFragment implements SearchView.OnQueryTextListener, AdapterView.OnItemClickListener {

    private ListView lvProp = null;
    private List<BuildPropInfo> list = null;
    private BuildPropAdapter adapter = null;
    private BuildPropLoader loader = null;
    private SearchView sv = null;
    private MenuItem miSave = null;
    private MenuItem miSearch = null;
    private LoadingView loading = null;

    @Override
    public int getBarTitle() {
        return R.string.fake_device_name;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        lvProp = (ListView) innerView.findViewById(R.id.lvProp);
        list = new ArrayList<>();
        adapter = new BuildPropAdapter(getContext(), list);
        lvProp.setAdapter(adapter);
        loader = new BuildPropLoader(getContext());
        loading = (LoadingView) innerView.findViewById(R.id.loading);
    }

    @Override
    public void initEvents() {
        lvProp.setOnItemClickListener(this);
        loader.registerListener(0, new Loader.OnLoadCompleteListener<List<BuildPropInfo>>() {
            @Override
            public void onLoadComplete(Loader<List<BuildPropInfo>> loader, List<BuildPropInfo> data) {
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
        return R.layout.fragment_fakedev;
    }

    @Override
    public String getMainActivityName() {
        return null;
    }

    @Override
    public void initMenu(Menu menu) {
        miSearch = menu.add(0, 1, 1, R.string.ab_search);
        miSearch.setIcon(android.R.drawable.ic_menu_search);
        miSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

        sv = new SearchView(getContext());
        sv.setOnQueryTextListener(this);
        miSearch.setActionView(sv);

        miSave = menu.add(0, 2, 2, R.string.ab_save);
        miSave.setIcon(android.R.drawable.ic_menu_save);
        miSave.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case 2:
                threadSaveBuildProp();
                break;
        }
        return true;
    }

    private Handler hSaving = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            Toast.makeText(getContext(), (msg.what == 0 ? R.string.toast_buildprop_saved : R.string.toast_buildprop_save_failed), Toast.LENGTH_SHORT).show();
            loading.setVisibility(View.GONE);
            super.handleMessage(msg);
        }
    };

    private void threadSaveBuildProp() {
        loading.setVisibility(View.VISIBLE);
        new Thread(new Runnable() {
            @Override
            public void run() {
                boolean ret = BuildPropUtils.setBuildProp(list);
                hSaving.sendEmptyMessage(ret ? 0 : 1);
            }
        }).start();
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
        final BuildPropInfo item = (BuildPropInfo) lvProp.getItemAtPosition(position);
        Intent inEdit = new Intent(getActivity(), BuildPropEditActivity.class);
        inEdit.putExtra("item", item);
        inEdit.putExtra("position", list.indexOf(item));
        startActivityForResult(inEdit, 0);
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (resultCode != Activity.RESULT_OK) {
            return;
        }
        switch (requestCode) {
            case 0:
                BuildPropInfo item = (BuildPropInfo) data.getSerializableExtra("item");
                int position = data.getIntExtra("position", -1);
                if (position != -1) {
                    list.set(position, item);
                    adapter.setNewList(list);
                    adapter.filter(sv.getQuery().toString());
                }
                break;
        }
    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        if (adapter != null) {
            adapter.filter(newText);
        }
        return false;
    }
}
