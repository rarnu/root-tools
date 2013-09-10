package com.rarnu.adcenter.classes;

import java.io.Serializable;

import org.json.JSONObject;

public class LoginItem implements Serializable {

	private static final long serialVersionUID = -8250094404420256214L;
	public int id;
	public String account;

	public static LoginItem fromJson(JSONObject json) throws Exception {
		LoginItem item = new LoginItem();
		item.id = json.getInt("id");
		item.account = json.getString("account");
		return item;
	}
}
