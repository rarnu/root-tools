package com.rarnu.tools.root.fragment;

import android.app.Instrumentation;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.Toast;

import com.rarnu.command.emu.EmulatorView;
import com.rarnu.command.emu.event.ITerminalCallback;
import com.rarnu.command.emu.tool.TermKeyListener;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.utils.MiscUtils;
import com.rarnu.devlib.utils.UIUtils;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.MenuItemIds;

public class TerminalFragment extends BaseFragment implements ITerminalCallback {

	EmulatorView emu;
	TermKeyListener mKeyListener;
	MenuItem itemSendCtrl, itemToggleInputMethod;

	@Override
	protected int getBarTitle() {
		return R.string.func_terminal;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.func_terminal_with_path;
	}

	@Override
	protected void initComponents() {
		emu = (EmulatorView) innerView.findViewById(R.id.emu);
		
		int actionBarHeight = UIUtils.getActionBarHeight();
		int statusBarHeight = UIUtils.getStatusbarHeight(getActivity());
		emu.setTitleHeight(actionBarHeight + statusBarHeight);
		emu.startListening("su\rclear\r");
		emu.setTextSize(16);
		mKeyListener = new TermKeyListener();
	}

	@Override
	protected void initEvents() {
		emu.register(this, mKeyListener);
	}

	@Override
	public void onPause() {
		emu.onPause();
		super.onPause();
	}

	@Override
	public void onResume() {
		super.onResume();
		emu.onResume();
	}
	
	@Override
	public void onDetach() {
		emu.close();
		super.onDetach();
	}

	@Override
	protected void initLogic() {
		emu.setFocusable(true);
		emu.setFocusableInTouchMode(true);
		emu.requestFocus();
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_terminal;
	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected void initMenu(Menu menu) {
		itemSendCtrl = menu.add(0, MenuItemIds.MENU_SEND_CTRL, 99,
				R.string.send_ctrl);
		itemSendCtrl.setIcon(android.R.drawable.ic_menu_slideshow);
		itemSendCtrl.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);

		itemToggleInputMethod = menu.add(0,
				MenuItemIds.MENU_TOGGLE_INPUT_METHOD, 100,
				R.string.toggle_input_method);
		itemToggleInputMethod
				.setIcon(android.R.drawable.ic_menu_sort_alphabetically);
		itemToggleInputMethod.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuItemIds.MENU_SEND_CTRL:
			Toast.makeText(getActivity(), R.string.send_ctrl,
					Toast.LENGTH_SHORT).show();
			new Thread(new Runnable() {

				@Override
				public void run() {
					Instrumentation inst = new Instrumentation();
					inst.sendKeyDownUpSync(KeyEvent.KEYCODE_DPAD_CENTER);
				}
			}).start();

			break;
		case MenuItemIds.MENU_TOGGLE_INPUT_METHOD:
			MiscUtils.toggleSoftKeyboard(getActivity());
			break;
		}
		return true;
	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	@Override
	public int getControlKeyCode() {
		return KeyEvent.KEYCODE_DPAD_CENTER;
	}
}
