package com.sbbs.me.android.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class HtmlUtils {

	public static List<String> getImages(String html) {
		String img = "";
		Pattern pImage;
		Matcher mImage;
		List<String> pics = new ArrayList<String>();
		String regEx_img = "<img.*src=(.*?)[^>]*?>";
		pImage = Pattern.compile(regEx_img, Pattern.CASE_INSENSITIVE);
		mImage = pImage.matcher(html);
		while (mImage.find()) {
			img = img + "," + mImage.group();
			Matcher m = Pattern.compile("src=\"?(.*?)(\"|>|\\s+)").matcher(img);
			while (m.find()) {
				pics.add(m.group(1));
			}
		}
		return pics;
	}

	public static class AHref {
		public String text;
		public String url;

		public AHref(String text, String url) {
			this.text = text;
			this.url = url;
		}
	}

	public static List<AHref> getUrls(String html) {
		List<String> listText = new ArrayList<String>();
		List<String> listUrl = new ArrayList<String>();

		String patternString = "\\s*(?i)href\\s*=\\s*(\"([^\"]*\")|'[^']*'|([^'\">\\s]+))"; // href
		Pattern pattern = Pattern.compile(patternString,
				Pattern.CASE_INSENSITIVE);
		Matcher matcher = pattern.matcher(html);

		while (matcher.find()) {
			String link = matcher.group();
			link = link.replaceAll("href\\s*=\\s*(['|\"]*)", "");
			link = link.replaceAll("['|\"]", "");
			link = link.trim();
			listUrl.add(link);
		}

		String patternStrs = "(<a.+?>)(.+?)(</a>)";
		pattern = Pattern.compile(patternStrs);
		matcher = pattern.matcher(html);
		while (matcher.find()) {
			listText.add(matcher.group(2));
		}
		List<AHref> list = new ArrayList<AHref>();
		for (int i = 0; i < listUrl.size(); i++) {
			list.add(new AHref(listText.get(i), listUrl.get(i)));
		}
		return list;

	}

}
