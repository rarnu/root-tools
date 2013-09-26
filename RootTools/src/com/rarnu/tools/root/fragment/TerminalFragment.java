package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.terminal.EmulatorView;
import com.rarnu.terminal.session.ShellTermSession;
import com.rarnu.terminal.session.TermSession;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.utils.MiscUtils;
import com.rarnu.utils.UIUtils;

public class TerminalFragment extends BaseFragment {

    EmulatorView emu;
    TermSession session;
    MenuItem itemSendCtrl, itemSendFn, itemToggleInputMethod;

    @Override
    public int getBarTitle() {
        return R.string.func_terminal;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.func_terminal_with_path;
    }

    @Override
    public void initComponents() {
        emu = (EmulatorView) innerView.findViewById(R.id.emu);

        session = new ShellTermSession("");
        session.setDefaultUTF8Mode(true);
        emu.attachSession(session);

        emu.setDensity(UIUtils.getDM());
        emu.setTextSize(10);

    }

    @Override
    public void onDestroy() {
        session.finish();
        super.onDestroy();
    }

    @Override
    public void initEvents() {

    }

    @Override
    public void initLogic() {

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_terminal;
    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void initMenu(Menu menu) {
        itemSendCtrl = menu.add(0, MenuItemIds.MENU_SEND_CTRL, 98, R.string.send_ctrl);
        itemSendCtrl.setTitle(R.string.ctrl);
        itemSendCtrl.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

        itemSendFn = menu.add(0, MenuItemIds.MENU_SEND_FN, 99, R.string.send_fn);
        itemSendFn.setTitle(R.string.fn);
        itemSendFn.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

        itemToggleInputMethod = menu.add(0, MenuItemIds.MENU_TOGGLE_INPUT_METHOD, 100, R.string.toggle_input_method);
        itemToggleInputMethod.setIcon(android.R.drawable.ic_menu_sort_alphabetically);
        itemToggleInputMethod.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuItemIds.MENU_SEND_CTRL:
                emu.sendControlKey();
                break;
            case MenuItemIds.MENU_SEND_FN:
                emu.sendFnKey();
                break;
            case MenuItemIds.MENU_TOGGLE_INPUT_METHOD:
                MiscUtils.toggleSoftKeyboard(getActivity());
                break;
        }
        return true;
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
