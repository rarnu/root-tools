package com.rarnu.tools.root.fragment;

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
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.BuildPropAdapter;
import com.rarnu.tools.root.common.BuildPropInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.fragmentactivity.BuildPropEditActivity;
import com.rarnu.tools.root.loader.BuildPropLoader;
import com.rarnu.tools.root.utils.BuildPropUtils;

import java.util.ArrayList;
import java.util.List;

public class BuildPropFragment extends BaseFragment implements Loader.OnLoadCompleteListener<List<BuildPropInfo>>, SearchView.OnQueryTextListener, AdapterView.OnItemClickListener {

    ListView lvBuildProp;
    List<BuildPropInfo> list;
    BuildPropAdapter adapter;
    BuildPropLoader loader;
    SearchView sv;
    MenuItem miSave;
    DataProgressBar progressBuild;

    private Handler hSaving = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1 && getActivity() != null) {
                Toast.makeText(getActivity(), (msg.arg1 == 0 ? R.string.build_prop_saving_succ : R.string.build_prop_saving_failed), Toast.LENGTH_SHORT).show();
                progressBuild.setVisibility(View.GONE);
            }
            super.handleMessage(msg);
        }
    };

    @Override
    public int getBarTitle() {
        return R.string.func_build_prop;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_build_prop_with_path;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        lvBuildProp = (ListView) innerView.findViewById(R.id.lvBuildProp);
        list = new ArrayList<BuildPropInfo>();
        adapter = new BuildPropAdapter(getActivity(), list);
        lvBuildProp.setAdapter(adapter);
        loader = new BuildPropLoader(getActivity());
        sv = (SearchView) innerView.findViewById(R.id.sv);
        progressBuild = (DataProgressBar) innerView.findViewById(R.id.progressBuild);
    }

    @Override
    public void initEvents() {
        lvBuildProp.setOnItemClickListener(this);
        loader.registerListener(0, this);
        sv.setOnQueryTextListener(this);
    }

    @Override
    public void initLogic() {
        progressBuild.setAppName(getString(R.string.loading));
        progressBuild.setVisibility(View.VISIBLE);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_build_prop;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        miSave = menu.add(0, MenuItemIds.MENU_SAVE, 99, R.string.save);
        miSave.setIcon(android.R.drawable.ic_menu_save);
        miSave.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_SAVE:

                doSaveBuildPropT();
                break;
        }
        return true;
    }

    private void doSaveBuildPropT() {
        progressBuild.setAppName(getString(R.string.build_prop_saving));
        progressBuild.setVisibility(View.VISIBLE);
        new Thread(new Runnable() {
            @Override
            public void run() {
                boolean ret = BuildPropUtils.setBuildProp(list);
                Message msg = new Message();
                msg.what = 1;
                msg.arg1 = ret ? 0 : 1;
                hSaving.sendMessage(msg);
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
    public void onLoadComplete(Loader<List<BuildPropInfo>> loader, List<BuildPropInfo> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
            progressBuild.setVisibility(View.GONE);
        }
    }

    @Override
    public boolean onQueryTextChange(String newText) {
        if (adapter != null) {
            adapter.filter(newText);
        }
        return false;
    }

    @Override
    public boolean onQueryTextSubmit(String query) {
        return false;
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        final BuildPropInfo item = (BuildPropInfo) lvBuildProp.getItemAtPosition(position);
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
}
