package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.utils.RuntimeUtils;

public class RuntimeFragment extends BaseFragment {

    MenuItem miSwitch;
    TextView tvRuntime;

    boolean isArt;

    @Override
    public int getBarTitle() {
        return R.string.func_runtime;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_runtime_with_path;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        tvRuntime = (TextView) innerView.findViewById(R.id.tvRuntime);
    }

    @Override
    public void initEvents() {

    }

    @Override
    public void initLogic() {
        isArt = RuntimeUtils.isArtMode();
        tvRuntime.setText(getString(R.string.current_runtime, isArt ? "ART" : "Dalvik"));
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_runtime;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        miSwitch = menu.add(0, MenuItemIds.MENU_SWITCH_RUNTIME, 99, R.string.switch_runtime);
        miSwitch.setIcon(R.drawable.ic_menu_always_landscape_portrait);
        miSwitch.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_SWITCH_RUNTIME:
                // TODO: switch runtime
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
}
