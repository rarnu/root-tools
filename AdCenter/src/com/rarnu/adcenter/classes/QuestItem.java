package com.rarnu.adcenter.classes;

import java.io.Serializable;

import org.json.JSONArray;
import org.json.JSONObject;

public class QuestItem implements Serializable{

	private static final long serialVersionUID = 862584944447300105L;
	public int id;
	public int ad_id;
	public String quest;
	public String desc;
	public String[] options;
	public int answer;
	public int cost;
	/**
	 * 0: half screen, 1: full screen
	 */
	public int type;
	
	public static QuestItem fromJson(JSONObject json) throws Exception {
		QuestItem item = new QuestItem();
		item.id = json.getInt("id");
		item.ad_id = json.getInt("ad_id");
		item.quest = json.getString("quest");
		item.desc = json.getString("desc");
		item.answer = json.getInt("answer");
		item.cost = json.getInt("cost");
		item.type = json.getInt("type");
		JSONArray jarrOptions = json.getJSONArray("options");
		item.options = new String[jarrOptions.length()];
		for (int i=0; i<jarrOptions.length(); i++) {
			item.options[i] = jarrOptions.getString(i);
		}
		return item;
	}
}
