package com.rarnu.adcenter.fragment;

import android.app.Activity;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.EditText;
import android.widget.Toast;

import com.rarnu.adcenter.Global;
import com.rarnu.adcenter.R;
import com.rarnu.adcenter.classes.CommonResult;
import com.rarnu.adcenter.classes.UserItem;
import com.rarnu.adcenter.common.MenuIds;
import com.rarnu.adcenter.database.AdUtils;
import com.rarnu.adcenter.loader.RegisterLoader;
import com.rarnu.adcenter.loader.UserLoader;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;

public class RegisterFragment extends BaseFragment {

	MenuItem itemRegister;
	EditText etAccount;
	EditText etPassword;
	EditText etRepeatPassword;
	EditText etNickName;
	EditText etEmail;
	EditText etPhone;
	EditText etQQNumber;

	RegisterLoader registerLoader;
	UserLoader userLoader;

	OnLoadCompleteListener<CommonResult> registerListener;
	OnLoadCompleteListener<UserItem> userListener;

	public RegisterFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_register);
	}

	@Override
	public int getBarTitle() {
		return R.string.register;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.register;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		etAccount = (EditText) innerView.findViewById(R.id.etAccount);
		etPassword = (EditText) innerView.findViewById(R.id.etPassword);
		etRepeatPassword = (EditText) innerView
				.findViewById(R.id.etRepeatPassword);
		etNickName = (EditText) innerView.findViewById(R.id.etNickName);
		etEmail = (EditText) innerView.findViewById(R.id.etEmail);
		etPhone = (EditText) innerView.findViewById(R.id.etPhone);
		etQQNumber = (EditText) innerView.findViewById(R.id.etQQNumber);
		registerLoader = new RegisterLoader(getActivity());
		userLoader = new UserLoader(getActivity());

		registerListener = new OnLoadCompleteListener<CommonResult>() {

			@Override
			public void onLoadComplete(Loader<CommonResult> loader,
					CommonResult data) {
				if (data != null) {
					userLoader.setUserId(data.result);
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
		registerLoader.registerListener(0, registerListener);
		userLoader.registerListener(0, userListener);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_register;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		itemRegister = menu.add(0, MenuIds.MENUID_REGISTER, 99,
				R.string.register);
		itemRegister.setIcon(android.R.drawable.ic_menu_send);
		itemRegister.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENUID_REGISTER:
			String account = etAccount.getText().toString();
			String passwd = etPassword.getText().toString();
			String passwdRepeat = etRepeatPassword.getText().toString();
			String name = etNickName.getText().toString();
			String email = etEmail.getText().toString();
			String phone = etPhone.getText().toString();
			String qq = etQQNumber.getText().toString();

			if (account.equals("") || passwd.equals("")
					|| passwdRepeat.equals("") || name.equals("")
					|| email.equals("") || phone.equals("")) {
				Toast.makeText(getActivity(), R.string.account_password_empty,
						Toast.LENGTH_LONG).show();
				return true;
			}
			if (!passwd.equals(passwdRepeat)) {
				Toast.makeText(getActivity(),
						R.string.account_password_different, Toast.LENGTH_LONG)
						.show();
				return true;
			}
			registerLoader.setData(account, passwd, name, Global.MAC_ADDRESS,
					email, phone, qq);
			registerLoader.startLoading();
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
