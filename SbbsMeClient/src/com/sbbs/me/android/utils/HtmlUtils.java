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
}
