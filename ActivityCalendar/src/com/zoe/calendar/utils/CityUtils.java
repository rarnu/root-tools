package com.zoe.calendar.utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import android.content.Context;

import com.rarnu.utils.FileUtils;
import com.zoe.calendar.classes.CityItem;

public class CityUtils {

	private static List<CityItem> listCities = new ArrayList<CityItem>();

	public static List<CityItem> loadCity(Context context) {

		if (listCities.size() != 0) {
			return listCities;
		}

		listCities.clear();
		try {
			List<String> listCity = FileUtils.readAssertFileAsList(context,
					"city");
			listCities = new ArrayList<CityItem>();
			for (String s : listCity) {
				listCities.add(new CityItem(s.substring(0, s.indexOf("=")), s
						.substring(s.indexOf("=") + 1)));
			}
		} catch (IOException e) {
		}
		return listCities;
	}

	public static CityItem findCity(String city) {
		CityItem item = null;
		for (CityItem ci : listCities) {
			if (city.startsWith(ci.name)) {
				item = ci;
				break;
			}
		}
		return item;
	}
}
