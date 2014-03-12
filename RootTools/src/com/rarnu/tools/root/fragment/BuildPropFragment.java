package com.rarnu.tools.root.fragment;

import android.content.Loader;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.SearchView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.BuildPropAdapter;
import com.rarnu.tools.root.common.BuildPropInfo;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.loader.BuildPropLoader;

import java.util.ArrayList;
import java.util.List;

public class BuildPropFragment extends BaseFragment implements Loader.OnLoadCompleteListener<List<BuildPropInfo>>, SearchView.OnQueryTextListener, AdapterView.OnItemClickListener {

    ListView lvBuildProp;
    List<BuildPropInfo> list;
    BuildPropAdapter adapter;
    BuildPropLoader loader;
    SearchView sv;
    MenuItem miSave;

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
    }

    @Override
    public void initEvents() {
        lvBuildProp.setOnItemClickListener(this);
        loader.registerListener(0, this);
        sv.setOnQueryTextListener(this);
    }

    @Override
    public void initLogic() {
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
                // TODO: save build.prop
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
    public void onLoadComplete(Loader<List<BuildPropInfo>> loader, List<BuildPropInfo> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
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
        // TODO: change item's value
    }
}
