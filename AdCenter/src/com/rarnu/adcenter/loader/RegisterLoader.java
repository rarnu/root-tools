package com.rarnu.adcenter.loader;

import android.content.Context;

import com.rarnu.adcenter.api.AdAPI;
import com.rarnu.adcenter.classes.CommonResult;
import com.rarnu.devlib.base.BaseClassLoader;

public class RegisterLoader extends BaseClassLoader<CommonResult> {

	String account, passwd, name, mac, email, phone, qq;

	public RegisterLoader(Context context) {
		super(context);
	}

	public void setData(String account, String passwd, String name, String mac,
			String email, String phone, String qq) {
		this.account = account;
		this.passwd = passwd;
		this.name = name;
		this.mac = mac;
		this.email = email;
		this.phone = phone;
		this.qq = qq;
	}

	@Override
	public CommonResult loadInBackground() {
		return AdAPI.register(account, passwd, name, mac, email, phone, qq);
	}

}
