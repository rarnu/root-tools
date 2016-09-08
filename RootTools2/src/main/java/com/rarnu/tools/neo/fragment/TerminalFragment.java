package com.rarnu.tools.neo.fragment;

import android.content.Context;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.inputmethod.InputMethodManager;
import com.rarnu.terminal.EmulatorView;
import com.rarnu.terminal.session.ShellTermSession;
import com.rarnu.terminal.session.TermSession;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.base.BaseFragment;
import com.rarnu.tools.neo.utils.UIUtils;

public class TerminalFragment extends BaseFragment  {

    private EmulatorView emu = null;
    private TermSession session = null;
    private MenuItem itemSendCtrl = null;
    private MenuItem itemSendFn = null;
    private MenuItem itemToggleInputMethod = null;

    @Override
    public int getBarTitle() {
        return R.string.terminal_name;
    }

    @Override
    public String getCustomTitle() {
        return null;
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
        toggleSoftKeyboard();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_terminal;
    }

    @Override
    public String getMainActivityName() {
        return null;
    }

    @Override
    public void initMenu(Menu menu) {
        menu.clear();
        itemToggleInputMethod = menu.add(0, 1, 1, R.string.ab_toggle_input_method);
        itemToggleInputMethod.setIcon(android.R.drawable.ic_menu_sort_alphabetically);
        itemToggleInputMethod.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        itemSendCtrl = menu.add(0, 2, 2, R.string.ab_send_ctrl);
        itemSendCtrl.setTitle(R.string.ab_ctrl);
        itemSendCtrl.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        itemSendFn = menu.add(0, 3, 3, R.string.ab_send_fn);
        itemSendFn.setTitle(R.string.ab_fn);
        itemSendFn.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case 1:
                toggleSoftKeyboard();
                break;
            case 2:
                emu.sendControlKey();
                break;
            case 3:
                emu.sendFnKey();
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

    private void toggleSoftKeyboard() {
        InputMethodManager imm = (InputMethodManager) getContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        imm.toggleSoftInput(InputMethodManager.SHOW_FORCED, 0);
    }
}
