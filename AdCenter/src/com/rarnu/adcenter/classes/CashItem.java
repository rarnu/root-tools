package com.rarnu.adcenter.classes;

import org.json.JSONObject;

public class CashItem {

	public int cash;
	public int real_money;
	public double rate;

	public static CashItem fromJson(JSONObject json) throws Exception {
		CashItem item = new CashItem();
		item.cash = json.getInt("cash");
		item.real_money = json.getInt("real_money");
		item.rate = json.getDouble("rate");
		return item;
	}
}
