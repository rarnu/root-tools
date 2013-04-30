package com.yugioh.android.utils;

import java.util.ArrayList;
import java.util.List;

import org.apache.http.protocol.HTTP;
import org.json.JSONArray;
import org.json.JSONObject;

import android.content.Context;

import com.rarnu.devlib.utils.HttpRequest;
import com.yugioh.android.classes.RecommandInfo;
import com.yugioh.android.classes.UpdateInfo;
import com.yugioh.android.define.NetworkDefine;

public class YGOAPI {

	public static UpdateInfo findUpdate(Context context, int lastCardId) {
		String param = String.format(NetworkDefine.UPDATE_PARAM_FMT,
				DeviceUtils.getAppVersionCode(context), lastCardId);
		UpdateInfo ui = null;
		try {
			String jsonstr = HttpRequest.get(NetworkDefine.UPDATE_URL, param,
					HTTP.UTF_8);
			JSONObject json = new JSONObject(jsonstr);
			ui = new UpdateInfo();
			ui.setUpdateApk(json.getInt("apk"));
			ui.setUpdateData(json.getInt("data"));
			ui.setNewCard(json.getInt("newcard"));
			ui.setApkVersion(json.getString("apkversion"));
		} catch (Exception e) {

		}
		return ui;

	}

	public static List<RecommandInfo> getRecommands() {
		List<RecommandInfo> list = null;
		try {
			String ret = HttpRequest.get(NetworkDefine.RECOMMAND_URL, "",
					HTTP.UTF_8);
			JSONObject json = new JSONObject(ret);
			JSONArray jarr = json.getJSONArray("data");
			list = new ArrayList<RecommandInfo>();
			for (int i = 0; i < jarr.length(); i++) {
				RecommandInfo item = new RecommandInfo();
				item.id = jarr.getJSONObject(i).getInt("id");
				item.name = jarr.getJSONObject(i).getString("name");
				item.jumpMode = jarr.getJSONObject(i).getInt("jump_mode");
				item.jumpUrl = jarr.getJSONObject(i).getString("jump_url");
				item.jumpText = jarr.getJSONObject(i).getString("jump_text");
				item.imagePath = jarr.getJSONObject(i).getString("image_name");
				item.bigQR = jarr.getJSONObject(i).getString("big_qr");
				list.add(item);
			}
		} catch (Exception e) {

		}
		return list;
	}
}
