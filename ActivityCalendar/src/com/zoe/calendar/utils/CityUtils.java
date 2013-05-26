package com.zoe.calendar.utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;

import com.rarnu.utils.FileUtils;
import com.zoe.calendar.classes.CityItem;

public class CityUtils {

	public static List<CityItem> loadCity(Context context) {
		List<CityItem> list = null;
		try {
			List<String> listCity = FileUtils.readAssertFileAsList(context,
					"city");
			list = new ArrayList<CityItem>();
			for (String s : listCity) {
				list.add(new CityItem(s.substring(0, s.indexOf("=")), s
						.substring(s.indexOf("=") + 1)));
			}
		} catch (IOException e) {
		}
		return list;
	}
}
