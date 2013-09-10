package com.rarnu.adcenter.classes;

import java.io.Serializable;

import org.json.JSONObject;

public class UserItem implements Serializable {

	private static final long serialVersionUID = 8716348352386162238L;
	public int id;
	public String account;
	public String name;
	public String email;
	public String phone;
	public String taobao_account;
	public String qq;
	public int cash;
	public int click_count;
	public int quest_count;
	public int quest_right;
	public int quest_wrong;

	public static UserItem fromJson(JSONObject json) throws Exception {
		UserItem item = new UserItem();
		item.id = json.getInt("id");
		item.account = json.getString("account");
		item.name = json.getString("name");
		item.email = json.getString("email");
		item.phone = json.getString("phone");
		item.taobao_account = json.getString("taobao_account");
		item.qq = json.getString("qq");
		item.cash = json.getInt("cash");
		item.click_count = json.getInt("click_count");
		item.quest_count = json.getInt("quest_count");
		item.quest_right = json.getInt("quest_right");
		item.quest_wrong = json.getInt("quest_wrong");
		return item;
	}
}
