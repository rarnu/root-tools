package com.yugioh.android.define;

import java.util.ArrayList;
import java.util.List;

public class CardConstDefine {

	public static final int DEFID_CARDRACE = 1;
	public static final int DEFID_CARDBELONGS = 2;
	public static final int DEFID_CARDTYPE = 3;
	public static final int DEFID_CARDATTRITUBE = 4;
	public static final int DEFID_CARDLEVEL = 5;
	public static final int DEFID_CARDRARE = 6;
	public static final int DEFID_CARDLIMIT = 7;
	public static final int DEFID_CARDTUNNER = 8;

	public static List<String> getCardRace() {
		return getDefine(CardRace);
	}

	public static List<String> getCardBelongs() {
		return getDefine(CardBelongs);
	}

	public static List<String> getCardType() {
		return getDefine(CardType);
	}

	public static List<String> getCardAttribute() {
		return getDefine(CardAttribute);
	}

	public static List<String> getCardLevel() {
		return getDefine(CardLevel);
	}

	public static List<String> getCardCare() {
		return getDefine(CardRare);
	}
	
	public static List<String> getCardLimit() {
		return getDefine(CardLimit);
	}
	
	public static List<String> getCardTunner() {
		return getDefine(CardTunner);
	}

	private static List<String> getDefine(String[] def) {
		List<String> list = new ArrayList<String>();
		for (String s : def) {
			list.add(s);
		}
		return list;
	}

	private static final String[] CardRace = { "战士", "不死", "恶魔", "兽", "兽战士",
			"恐龙", "鱼", "机械", "水", "天使", "魔法师", "炎", "雷", "爬虫类", "植物", "昆虫",
			"岩石", "海龙", "鸟兽", "龙", "念动力", "幻龙", "幻神兽" };

	private static final String[] CardBelongs = { "OCG、TCG", "OCG", "TCG" };

	private static final String[] CardType = { "怪兽", "通常怪兽", "效果怪兽", "同调怪兽",
			"融合怪兽", "仪式怪兽", "XYZ怪兽", "魔法", "通常魔法", "场地魔法", "速攻魔法", "装备魔法",
			"仪式魔法", "永续魔法", "陷阱", "通常陷阱", "反击陷阱", "永续陷阱" };

	private static final String[] CardAttribute = { "地", "水", "炎", "风", "光",
			"暗", "神" };

	private static final String[] CardLevel = { "1", "2", "3", "4", "5", "6",
			"7", "8", "9", "10", "11", "12" };

	private static final String[] CardRare = { "平卡N", "银字R", "黄金GR", "平罕NR",
			"面闪SR", "金字UR", "爆闪PR", "全息HR", "鬼闪GHR", "平爆NPR", "红字RUR", "斜碎SCR",
			"银碎SER", "金碎USR", "立体UTR" };

	private static final String[] CardLimit = { "无限制", "禁止卡", "限制卡", "准限制卡",
			"观赏卡" };

    private static final String[] CardTunner = {"卡通","灵魂","同盟","调整","二重","反转","灵摆"};
}
