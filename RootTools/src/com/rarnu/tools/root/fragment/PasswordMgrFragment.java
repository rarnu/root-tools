package com.rarnu.tools.root.fragment;

import android.app.Activity;
import android.content.Intent;
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
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.fragmentactivity.PasswordChangeSecActivity;
import com.rarnu.tools.root.fragmentactivity.PasswordSecActivity;

public class PasswordMgrFragment extends BaseFragment implements View.OnClickListener, AdapterView.OnItemClickListener {

    RelativeLayout layReauth;
    TextView tvReauth;
    MenuItem miSettings;
    MenuItem miAdd;
    ListView lvPassword;

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
    }

    @Override
    public void initEvents() {
        tvReauth.setOnClickListener(this);
        lvPassword.setOnItemClickListener(this);
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
        }


    }

    private void doStartLoading() {
        // TODO: start loading
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
                // TODO: add password item
                break;
            case MenuItemIds.MENU_SETTINGS:
                startActivity(new Intent(getActivity(), PasswordChangeSecActivity.class));
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
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.tvReauth:
                startActivityForResult(new Intent(getActivity(), PasswordSecActivity.class), 0);
                break;
        }
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        // TODO: goto password detail
    }
}
