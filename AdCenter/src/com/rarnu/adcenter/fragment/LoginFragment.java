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
import android.widget.EditText;
import android.widget.Toast;

import com.rarnu.adcenter.Global;
import com.rarnu.adcenter.R;
import com.rarnu.adcenter.RegisterActivity;
import com.rarnu.adcenter.classes.LoginItem;
import com.rarnu.adcenter.classes.UserItem;
import com.rarnu.adcenter.common.MenuIds;
import com.rarnu.adcenter.database.AdUtils;
import com.rarnu.adcenter.loader.LoginLoader;
import com.rarnu.adcenter.loader.UserLoader;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;

public class LoginFragment extends BaseFragment implements OnClickListener {

	EditText etAccount;
	EditText etPassword;
	LoginLoader loginLoader;
	UserLoader userLoader;
	MenuItem itemLogin;
	Button btnRegister;

	OnLoadCompleteListener<LoginItem> loginListener;
	OnLoadCompleteListener<UserItem> userListener;

	public LoginFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_login);
	}

	@Override
	public int getBarTitle() {
		return R.string.login;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.login;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		etAccount = (EditText) innerView.findViewById(R.id.etAccount);
		etPassword = (EditText) innerView.findViewById(R.id.etPassword);
		loginLoader = new LoginLoader(getActivity());
		userLoader = new UserLoader(getActivity());
		btnRegister = (Button) innerView.findViewById(R.id.btnRegister);

		loginListener = new OnLoadCompleteListener<LoginItem>() {
			@Override
			public void onLoadComplete(Loader<LoginItem> loader, LoginItem data) {
				if (data != null) {
					userLoader.setUserId(data.id);
					userLoader.startLoading();
				}
			}
		};

		userListener = new OnLoadCompleteListener<UserItem>() {

			@Override
			public void onLoadComplete(Loader<UserItem> loader, UserItem data) {
				if (data != null) {
					AdUtils.login(getActivity(), data);
					getActivity().setResult(Activity.RESULT_OK);
					getActivity().finish();
				}
			}
		};

	}

	@Override
	public void initEvents() {
		loginLoader.registerListener(0, loginListener);
		userLoader.registerListener(0, userListener);
		btnRegister.setOnClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_login;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		itemLogin = menu.add(0, MenuIds.MENUID_LOGIN, 99, R.string.login);
		itemLogin.setIcon(android.R.drawable.ic_menu_send);
		itemLogin.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENUID_LOGIN:
			String account = etAccount.getText().toString();
			String passwd = etPassword.getText().toString();
			if (account.equals("") || passwd.equals("")) {
				Toast.makeText(getActivity(), R.string.account_password_empty,
						Toast.LENGTH_LONG).show();
				return true;
			}
			loginLoader.setData(account, passwd, Global.MAC_ADDRESS);
			loginLoader.startLoading();
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
		case R.id.btnRegister:
			startActivityForResult(new Intent(getActivity(),
					RegisterActivity.class), 0);
			break;
		}
	}
	
	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode == Activity.RESULT_OK) {
			getActivity().setResult(Activity.RESULT_OK);
			getActivity().finish();
			return;
		}
	}

}
