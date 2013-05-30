package com.zoe.calendar.utils;

import java.util.Calendar;

import android.content.Context;

import com.zoe.calendar.Global;
import com.zoe.calendar.classes.WeatherInfo;
import com.zoe.calendar.common.Config;

public class WeatherUtils {

	public static void saveWeather(Context context, WeatherInfo weather,
			long timestamp) {
		Config.setLastWeatherTimestamp(context, Global.city_pinyin, timestamp);
		Config.setLastWeatherString(context, Global.city_pinyin,
				weather.weather);
		Config.setLastWeatherTemp(context, Global.city_pinyin, weather.temp);
	}

	public static WeatherInfo loadLocalWeather(Context context) {
		long timestamp = Config.getLastWeatherTimestamp(context, Global.city_pinyin);
		WeatherInfo wi = null;
		if (timestamp != 0) {
			// load weather
			Calendar cNow = Calendar.getInstance();
			Calendar cLocal = Calendar.getInstance();
			cLocal.setTimeInMillis(timestamp);
			if (DateUtils.hoursBetween(cNow, cLocal) <= 12) {
				wi = new WeatherInfo();
				wi.weather = Config.getLastWeatherString(context, Global.city_pinyin);
				wi.temp = Config.getLastWeatherTemp(context, Global.city_pinyin);
			}
		} 
		return wi;
	}
}
