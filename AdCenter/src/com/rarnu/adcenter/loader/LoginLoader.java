package com.rarnu.adcenter.loader;

import android.content.Context;

import com.rarnu.adcenter.api.AdAPI;
import com.rarnu.adcenter.classes.LoginItem;
import com.rarnu.devlib.base.BaseClassLoader;

public class LoginLoader extends BaseClassLoader<LoginItem> {

	private String account;
	private String passwd;
	private String mac;
	
	public LoginLoader(Context context) {
		super(context);
	}
	
	public void setData(String account, String passwd, String mac) {
		this.account = account;
		this.passwd = passwd;
		this.mac = mac;
	}

	@Override
	public LoginItem loadInBackground() {
		return AdAPI.login(account, passwd, mac);
	}

}
