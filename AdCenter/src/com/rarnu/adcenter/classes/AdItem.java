package com.rarnu.adcenter.classes;

import java.io.Serializable;

import org.json.JSONObject;

public class AdItem implements Serializable {

	private static final long serialVersionUID = 4645666772800629296L;
	public int id;
	public String title;
	public String image_url;
	public int x_size;
	public int y_size;
	public String desc;
	public String qrcode_url;
	public String click_url;
	public int cost;

	public static AdItem fromJson(JSONObject json) throws Exception {
		AdItem item = new AdItem();
		item.id = json.getInt("id");
		item.title = json.getString("title");
		item.image_url = json.getString("image_url");
		item.x_size = json.getInt("x_size");
		item.y_size = json.getInt("y_size");
		item.desc = json.getString("desc");
		item.qrcode_url = json.getString("qrcode_url");
		item.click_url = json.getString("click_url");
		item.cost = json.getInt("cost");
		return item;
	}
}
