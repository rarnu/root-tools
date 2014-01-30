package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.adapter.PasswordAdapter;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.common.PasswordItem;
import com.rarnu.tools.root.fragmentactivity.PasswordChangeSecActivity;
import com.rarnu.tools.root.fragmentactivity.PasswordDetailActivity;
import com.rarnu.tools.root.fragmentactivity.PasswordSecActivity;
import com.rarnu.tools.root.loader.PasswordLoader;
import com.rarnu.utils.InputMethodUtils;

import java.util.ArrayList;
import java.util.List;

public class PasswordMgrFragment extends BaseFragment implements View.OnClickListener, AdapterView.OnItemClickListener, Loader.OnLoadCompleteListener<List<PasswordItem>> {

    RelativeLayout layReauth;
    TextView tvReauth;
    MenuItem miSettings;
    MenuItem miAdd;
    ListView lvPassword;
    PasswordAdapter adapter;
    List<PasswordItem> list;
    PasswordLoader loader;

    @Override
    public int getBarTitle() {
        return R.string.func_password;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_password_with_path;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        layReauth = (RelativeLayout) innerView.findViewById(R.id.layReauth);
        tvReauth = (TextView) innerView.findViewById(R.id.tvReauth);
        lvPassword = (ListView) innerView.findViewById(R.id.lvPassword);
        lvPassword.setVisibility(View.GONE);
        list = new ArrayList<PasswordItem>();
        adapter = new PasswordAdapter(getActivity(), list);
        lvPassword.setAdapter(adapter);
        loader = new PasswordLoader(getActivity());
    }

    @Override
    public void initEvents() {
        tvReauth.setOnClickListener(this);
        lvPassword.setOnItemClickListener(this);
        loader.registerListener(0, this);
    }

    @Override
    public void initLogic() {
        startActivityForResult(new Intent(getActivity(), PasswordSecActivity.class), 0);
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        switch (requestCode) {
            case 0:
                if (resultCode != Activity.RESULT_OK) {
                    layReauth.setVisibility(View.VISIBLE);
                    if (miSettings != null) {
                        miSettings.setEnabled(false);
                    }
                    if (miAdd != null) {
                        miAdd.setEnabled(false);
                    }
                    return;
                }
                layReauth.setVisibility(View.GONE);
                if (miAdd != null) {
                    miAdd.setEnabled(true);
                }
                if (miSettings != null) {
                    miSettings.setEnabled(true);
                }
                lvPassword.setVisibility(View.VISIBLE);
                doStartLoading();
                break;
            case 1:
                if (resultCode == Activity.RESULT_OK) {
                    boolean reload = data.getBooleanExtra("reload", false);
                    if (reload) {
                        doStartLoading();
                    }
                }
                break;
        }

    }

    private void doStartLoading() {
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_password_mgr;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        miAdd = menu.add(0, MenuItemIds.MENU_ADD, 98, R.string.add);
        miAdd.setIcon(android.R.drawable.ic_menu_add);
        miAdd.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        miSettings = menu.add(0, MenuItemIds.MENU_SETTINGS, 99, R.string.settings);
        miSettings.setIcon(android.R.drawable.ic_menu_manage);
        miSettings.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_ADD:
                openPasswordDetail(true, null);
                break;
            case MenuItemIds.MENU_SETTINGS:
                startActivity(new Intent(getActivity(), PasswordChangeSecActivity.class));
                break;
        }
        return true;
    }

    private void openPasswordDetail(boolean isAdd, PasswordItem item) {
        Intent inDetail = new Intent(getActivity(), PasswordDetailActivity.class);
        inDetail.putExtra("isAdd", isAdd);
        if (item != null) {
            inDetail.putExtra("item", item);
        }
        startActivityForResult(inDetail, 1);
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.tvReauth:
                startActivityForResult(new Intent(getActivity(), PasswordSecActivity.class), 0);
                break;
        }
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        PasswordItem item = (PasswordItem) lvPassword.getItemAtPosition(position);
        openPasswordDetail(false, item);
    }

    @Override
    public void onLoadComplete(Loader<List<PasswordItem>> loader, List<PasswordItem> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);
        }
        if (getActivity() != null) {
            adapter.setNewList(list);
        }
    }
}
