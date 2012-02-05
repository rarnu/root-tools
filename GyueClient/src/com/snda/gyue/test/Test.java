package com.snda.gyue.test;

import java.util.ArrayList;
import java.util.List;

import com.snda.gyue.classes.ArticleItem;

public class Test {

	public static List<ArticleItem> getTestArticles() {

		List<ArticleItem> list = new ArrayList<ArticleItem>();
		for (int i=0; i<10; i++) {
			list.add(makeItem(i));
		}
		return list;
	}

	private static ArticleItem makeItem(int index) {
		ArticleItem item = new ArticleItem();
		item.setId(index);
		item.setTitle(titles[index]);
		item.setAuthor(authors[index]);
		item.setDatetime(datetimes[index]);
		item.setTag(tags[index]);
		item.setHot(hots[index]);
		item.setRecommand(recommands[index]);
		return item;
	}
	
	private static String[] titles = new String[] {
		"HP TouchPad将快速得到Android 4.0",
		"YouTube占英国社交网访问量四分之一",
		"手游进入淘汰赛：海外变淘金主战场",
		"12306招标内幕：肥水不流外人田",
		"Android应用“安豆苗”用手指滑动图片至另一部手机",
		"土豆号召视频网站屏蔽优酷 新浪将加入",
		"谷歌地图趣味迷宫新广告 玩转生活",
		"Android智能眼罩式显示器Smart Goggles",
		"体验联想IdeaPad S2 7无线传输高清画质",
		"VeryCD起死回生 转型页游月入过亿"};
	
	private static String[] authors = new String[] {
		"stone119",
		"stone119",
		"stone119",
		"stone119",
		"stone119",
		"stone119",
		"stone119",
		"stone119",
		"stone119",
		"stone119"};
	private static String[] datetimes = new String[] {
		"2012-01-15 13:05",
		"2012-01-15 12:10",
		"2012-01-14 19:53",
		"2012-01-14 13:56",
		"2012-01-14 14:52",
		"2012-01-14 14:30",
		"2012-01-14 14:06",
		"2012-01-14 13:35",
		"2012-01-14 12:07",
		"2012-01-14 09:25"};
	
	private static String[] tags = new String[] {
		"HPTouchPad Android 4.0",
		"YouTube 英国 社交网",
		"手游 海外",
		"12306 招标 内幕",
		"Android 应用 安豆苗",
		"土豆 屏蔽 优酷",
		"谷歌 地图 趣味",
		"Android 智能 Smart Goggles",
		"联想 IdeaPad 无线",
		"VeryCD 转型 页游"};
	private static boolean[] hots = new boolean[] {
		false, 
		true,
		false,
		true,
		false,
		false,
		false,
		true,
		false,
		false
	};
	private static boolean[] recommands = new boolean[] {
		true,
		false,
		false,
		true,
		false,
		false,
		true,
		true,
		false,
		true
	};
}
