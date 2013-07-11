package com.sbbs.me.android.api;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONObject;

public class SbbsMeArticle {

	public SbbsMeBlock main_block;
	public List<SbbsMeBlock> sub_blocks;
	public Map<String, List<SbbsMeBlock>> left_blocks;
	public Map<String, List<SbbsMeBlock>> right_blocks;
	public List<SbbsMeTag> tags;
	public Map<String, String> users;

	public static SbbsMeArticle fromJson(JSONObject json) throws Exception {
		SbbsMeArticle article = new SbbsMeArticle();

		article.main_block = SbbsMeBlock.fromJson(json
				.getJSONObject("main_block"));

		JSONArray jArrTags = json.getJSONArray("tags");
		article.tags = new ArrayList<SbbsMeTag>();
		if (jArrTags != null && jArrTags.length() != 0) {
			for (int i = 0; i < jArrTags.length(); i++) {
				article.tags.add(SbbsMeTag.fromJson(jArrTags.getJSONObject(i)));
			}
		}

		JSONArray jArrSub = json.getJSONArray("sub_blocks");
		article.sub_blocks = new ArrayList<SbbsMeBlock>();
		if (jArrSub != null && jArrSub.length() != 0) {
			for (int i = 0; i < jArrSub.length(); i++) {
				article.sub_blocks.add(SbbsMeBlock.fromJson(jArrSub
						.getJSONObject(i)));
			}
		}

		List<String> blockIds = article.getBlockIds();
		if (blockIds != null && blockIds.size() != 0) {
			article.left_blocks = new HashMap<String, List<SbbsMeBlock>>();
			article.right_blocks = new HashMap<String, List<SbbsMeBlock>>();
			JSONObject jObjLeft = json.getJSONObject("left_blocks");
			JSONObject jObjRight = json.getJSONObject("right_blocks");
			for (int i = 0; i < blockIds.size(); i++) {
				JSONArray jArrBlockLeft = jObjLeft
						.getJSONArray(blockIds.get(i));
				JSONArray jArrBlockRight = jObjRight.getJSONArray(blockIds
						.get(i));
				List<SbbsMeBlock> listLeft = new ArrayList<SbbsMeBlock>();
				List<SbbsMeBlock> listRight = new ArrayList<SbbsMeBlock>();

				if (jArrBlockLeft != null && jArrBlockLeft.length() != 0) {
					for (int j = 0; j < jArrBlockLeft.length(); j++) {
						listLeft.add(SbbsMeBlock.fromJson(jArrBlockLeft
								.getJSONObject(j)));
					}
				}
				if (jArrBlockRight != null && jArrBlockRight.length() != 0) {
					for (int j = 0; j < jArrBlockRight.length(); j++) {
						listRight.add(SbbsMeBlock.fromJson(jArrBlockRight
								.getJSONObject(j)));
					}
				}
				article.left_blocks.put(blockIds.get(i), listLeft);
				article.right_blocks.put(blockIds.get(i), listRight);
			}
		}
		
		article.users = new HashMap<String, String>();
		JSONObject jObjUsers = json.getJSONObject("users");
		Iterator<?> iter = jObjUsers.keys();
		String key = "";
		String value = "";
		while (iter.hasNext()) {
			key = (String) iter.next();
			value = jObjUsers.getString(key);
			article.users.put(key, value);
		}
		return article;
	}

	private List<String> getBlockIds() {
		List<String> list = new ArrayList<String>();
		if (main_block != null) {
			list.add(main_block.Id);
		}
		if (sub_blocks != null && sub_blocks.size() != 0) {
			for (int i = 0; i < sub_blocks.size(); i++) {
				list.add(sub_blocks.get(i).Id);
			}
		}

		return list;
	}

}
