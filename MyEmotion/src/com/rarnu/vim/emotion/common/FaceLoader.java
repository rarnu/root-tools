package com.rarnu.vim.emotion.common;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import android.content.Context;

public class FaceLoader {

	public static List<EmotionFace> faceList = new ArrayList<EmotionFace>();

	public static void loadFace(Context context) {
		if (faceList.size() != 0) {
			return;
		}
		String jsonString = "";
		try {
			InputStream is = context.getAssets().open("face/facemap.json");
			BufferedReader br = new BufferedReader(new InputStreamReader(is));
			String line;
			while ((line = br.readLine()) != null) {
				jsonString += line + '\n';
			}

			JSONObject json = new JSONObject(jsonString);
			JSONArray arr = json.getJSONArray("face");
			JSONObject o;
			for (int i = 0; i < arr.length(); i++) {
				o = arr.getJSONObject(i);

				EmotionFace face = new EmotionFace();
				face.name = o.getString("name");
				face.value = o.getString("value");
				faceList.add(face);
			}

		} catch (Exception e) {
			
		}
	}
}
