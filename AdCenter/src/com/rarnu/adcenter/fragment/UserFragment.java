package com.rarnu.adcenter.fragment;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.RelativeLayout;
import android.widget.ScrollView;
import android.widget.TextView;

import com.rarnu.adcenter.LoginActivity;
import com.rarnu.adcenter.R;
import com.rarnu.adcenter.classes.UserItem;
import com.rarnu.adcenter.common.MenuIds;
import com.rarnu.adcenter.database.AdUtils;
import com.rarnu.adcenter.loader.UserLoader;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;

public class UserFragment extends BaseFragment implements
		OnLoadCompleteListener<UserItem>, OnClickListener {

	MenuItem itemEdit;
	UserItem user = null;
	TextView tvAccount;
	TextView tvName;
	TextView tvEmail;
	TextView tvPhone;
	TextView tvLoading;
	UserLoader loader;
	ScrollView svUser;
	RelativeLayout layLogin;
	Button btnLogin;
	Button btnLogout;
	
	int userId = 0;

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
		tvEmail = (TextView) innerView.findViewById(R.id.tvEmail);
		tvPhone = (TextView) innerView.findViewById(R.id.tvPhone);
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
		svUser = (ScrollView) innerView.findViewById(R.id.svUser);
		btnLogin = (Button) innerView.findViewById(R.id.btnLogin);
		btnLogout = (Button) innerView.findViewById(R.id.btnLogout);
		layLogin = (RelativeLayout) innerView.findViewById(R.id.layLogin);
		loader = new UserLoader(getActivity());
	}
	
	@Override
	public void initEvents() {
		loader.registerListener(0, this);
		btnLogin.setOnClickListener(this);
		btnLogout.setOnClickListener(this);
	}

	@Override
	public void initLogic() {
		loadUser();
		if (user != null) {
			svUser.setVisibility(View.VISIBLE);
			layLogin.setVisibility(View.GONE);
			tvLoading.setVisibility(View.VISIBLE);
			loader.setUserId(userId);
			loader.startLoading();
		} else {
			svUser.setVisibility(View.GONE);
			layLogin.setVisibility(View.VISIBLE);
		}
	}

	private void loadUser() {
		user = AdUtils.queryUser(getActivity());
		loadUserData();
	}

	private void loadUserData() {
		if (user != null) {
			userId = user.id;
			tvAccount.setText(user.account);
			tvName.setText(user.name);
			tvEmail.setText(user.email);
			tvPhone.setText(user.phone);
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
		itemEdit = menu.add(0, MenuIds.MENUID_EDIT, 99, R.string.login);
		itemEdit.setIcon(android.R.drawable.ic_menu_edit);
		itemEdit.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENUID_EDIT:
			if (user != null) {
				// TODO: edit user
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
			initLogic();
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
		
		svUser.setVisibility(View.GONE);
		layLogin.setVisibility(View.VISIBLE);
	}

	@Override
	public void onLoadComplete(Loader<UserItem> loader, UserItem data) {
		if (data != null) {
			user = data;
		}
		if (getActivity() != null) {
			loadUserData();
			tvLoading.setVisibility(View.GONE);
		}

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLogin:
			startActivityForResult(new Intent(getActivity(),
					LoginActivity.class), 0);
			break;
		case R.id.btnLogout:
			doLogout();
			break;
		}
		
	}

}
