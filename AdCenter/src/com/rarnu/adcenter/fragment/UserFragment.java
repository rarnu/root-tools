package com.rarnu.adcenter.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.TextView;

import com.rarnu.adcenter.LoginActivity;
import com.rarnu.adcenter.R;
import com.rarnu.adcenter.classes.UserItem;
import com.rarnu.adcenter.common.MenuIds;
import com.rarnu.adcenter.database.AdUtils;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;

public class UserFragment extends BaseFragment {

	MenuItem itemLogin;
	UserItem user = null;
	TextView tvAccount;
	TextView tvName;

	public UserFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_user);
	}

	@Override
	public int getBarTitle() {
		return R.string.user_account;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.user_account;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		tvAccount = (TextView) innerView.findViewById(R.id.tvAccount);
		tvName = (TextView) innerView.findViewById(R.id.tvName);
	}

	@Override
	public void initEvents() {

	}

	@Override
	public void initLogic() {
		loadUser();
	}
	
	
	private void loadUser() {
		user = AdUtils.queryUser(getActivity());
		if (user != null) {
			tvAccount.setText(user.account);
			tvName.setText(user.name);
		}
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_user;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		itemLogin = menu.add(0, MenuIds.MENUID_LOGIN, 99, R.string.login);
		itemLogin.setIcon(android.R.drawable.ic_menu_myplaces);
		itemLogin.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENUID_LOGIN:
			if (user != null) {
				doLogout();
			} else {
				startActivityForResult(new Intent(getActivity(),
						LoginActivity.class), 0);
			}
			break;
		}
		return true;
	}
	
	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != Activity.RESULT_OK) {
			return;
		}
		switch (requestCode) {
		case 0:
			loadUser();
			break;
		}
	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	private void doLogout() {
		AdUtils.logout(getActivity(), user);
		user = null;
		tvAccount.setText("");
		tvName.setText("");
	}

}
