package com.yugioh.android.utils;

import org.apache.http.protocol.HTTP;
import org.json.JSONObject;

import android.content.Context;

import com.rarnu.devlib.utils.HttpRequest;
import com.yugioh.android.classes.UpdateInfo;
import com.yugioh.android.define.NetworkDefine;

public class YGOAPI {

	public static final int MSG_DOWNLOAD_START = 10;
	public static final int MSG_DOWNLOAD_PROGRESS = 11;
	public static final int MSG_DOWNLOAD_FINISH = 12;
	public static final int MSG_DOWNLOAD_ERROR = 19;

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

}
