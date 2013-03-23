package com.rarnu.almanac;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

public class Almanac {

	private Calendar today = null;
	private int iday = 0;

	private String[] weeks = { "日", "一", "二", "三", "四", "五", "六" };
	private String[] directions = { "北方", "东北方", "东方", "东南方", "南方", "西南方",
			"西方", "西北方" };

	private String[] tools = { "Eclipse写程序", "MSOffice写文档", "记事本写程序",
			"Windows8", "Linux", "MacOS", "IE", "Android设备", "iOS设备" };

	private String[] varNames = { "jieguo", "huodong", "pay", "expire",
			"zhangdan", "every", "free", "i1", "a", "virtual", "ad", "spider",
			"mima", "pass", "ui" };

	private String[] drinks = { "水", "茶", "红茶", "绿茶", "咖啡", "奶茶", "可乐", "牛奶",
			"豆奶", "果汁", "果味汽水", "苏打水", "运动饮料", "酸奶", "酒" };

	public class Actions {
		public String name;
		public String good;
		public String bad;

		public Actions(String name, String good, String bad) {
			this.name = name;
			this.good = good;
			this.bad = bad;
		}
	}

	public class Result {
		public String name;
		public String desc;
		public boolean isGood;

		public Result(String name, String desc, boolean isGood) {
			this.name = name;
			this.desc = desc;
			this.isGood = isGood;
		}

		public String toString() {
			String ret = String.format("name:%s, desc:%s, isGood:%s", name,
					desc, (isGood ? "true" : "false"));
			return ret;
		}
	}

	private Actions[] activities = {
			new Actions("写单元测试", "写单元测试将减少出错", "写单元测试会降低你的开发效率"),
			new Actions("洗澡", "你几天没洗澡了？", "会把设计方面的灵感洗掉"),
			new Actions("锻炼一下身体", "", "能量没消耗多少，吃得却更多"),
			new Actions("抽烟", "抽烟有利于提神，增加思维敏捷", "除非你活够了，死得早点没关系"),
			new Actions("白天上线", "今天白天上线是安全的", "可能导致灾难性后果"),
			new Actions("重构", "代码质量得到提高", "你很有可能会陷入泥潭"),
			new Actions("使用%t", "你看起来更有品位", "别人会觉得你在装逼"),
			new Actions("跳槽", "该放手时就放手", "鉴于当前的经济形势，你的下一份工作未必比现在强"),
			new Actions("招人", "你遇到千里马的可能性大大增加", "你只会招到一两个混饭吃的外行"),
			new Actions("面试", "面试官今天心情很好", "面试官不爽，会拿你出气"),
			new Actions("提交辞职申请", "公司找到了一个比你更能干更便宜的家伙，巴不得你赶快滚蛋",
					"鉴于当前的经济形势，你的下一份工作未必比现在强"),
			new Actions("申请加薪", "老板今天心情很好", "公司正在考虑裁员"),
			new Actions("晚上加班", "晚上是程序员精神最好的时候", ""),
			new Actions("在妹子面前吹牛", "改善你矮穷挫的形象", "会被识破"),
			new Actions("撸管", "避免缓冲区溢出", "小撸怡情，大撸伤身，强撸灰飞烟灭"),
			new Actions("浏览成人网站", "重拾对生活的信心", "你会心神不宁"),
			new Actions("命名变量\"%v\"", "", ""),
			new Actions("写超过%l行的方法", "你的代码组织的很好，长一点没关系", "你的代码将混乱不堪，你自己都看不懂"),
			new Actions("提交代码", "遇到冲突的几率是最低的", "你遇到的一大堆冲突会让你觉得自己是不是时间穿越了"),
			new Actions("代码复审", "发现重要问题的几率大大增加", "你什么问题都发现不了，白白浪费时间"),
			new Actions("开会", "写代码之余放松一下打个盹，有益健康", "你会被扣屎盆子背黑锅"),
			new Actions("打DOTA", "你将有如神助", "你会被虐的很惨"),
			new Actions("晚上上线", "晚上是程序员精神最好的时候", "你白天已经筋疲力尽了"),
			new Actions("修复BUG", "你今天对BUG的嗅觉大大提高", "新产生的BUG将比修复的更多"),
			new Actions("设计评审", "设计评审会议将变成头脑风暴", "人人筋疲力尽，评审就这么过了"),
			new Actions("需求评审", "", ""),
			new Actions("上微博", "今天发生的事不能错过", "会被老板看到"),
			new Actions("上AB站", "还需要理由吗？", "会被老板看到") };

	public Almanac() {
		today = Calendar.getInstance();
		iday = today.get(Calendar.YEAR) * 10000
				+ (today.get(Calendar.MONTH) + 1) * 100
				+ today.get(Calendar.DATE);
	}

	public int random(int dayseed, int indexseed) {
		int n = dayseed % 11117;
		for (int i = 0; i < 100 + indexseed; i++) {
			n = n * n;
			n = n % 11117;
		}
		return n;
	}

	public String getTodayString() {
		return "今天是" + today.get(Calendar.YEAR) + "年"
				+ (today.get(Calendar.MONTH) + 1) + "月"
				+ today.get(Calendar.DATE) + "日 星期"
				+ weeks[today.get(Calendar.DAY_OF_WEEK) - 1];
	}

	public String getDirectionString() {
		return directions[random(iday, 2) % directions.length];
	}

	public String getDrinkString() {
		List<String> ret = pickRandomStr(drinks, 2);
		return ret.get(0) + "," + ret.get(1);
	}

	public String getGoddesString() {
		return String.valueOf(random(iday, 6) % 50 / 10.0);
	}

	public List<Result> pickTodaysLuck() {
		int numGood = random(iday, 98) % 3 + 2;
		int numBad = random(iday, 87) % 3 + 2;
		List<Actions> eventArr = pickRandomActivity(numGood + numBad);

		List<Result> list = new ArrayList<Result>();
		for (int i = 0; i < numGood; i++) {
			Actions a = eventArr.get(i);
			list.add(new Result(a.name, a.good, true));
		}

		for (int i = 0; i < numBad; i++) {
			Actions a = eventArr.get(numGood + i);
			list.add(new Result(a.name, a.bad, false));
		}

		return list;
	}

	// 从 activities 中随机挑选 size 个
	private List<Actions> pickRandomActivity(int size) {
		List<Actions> picked_events = pickRandom(activities, size);

		for (int i = 0; i < picked_events.size(); i++) {
			parse(picked_events.get(i));
		}

		return picked_events;
	}

	// 从数组中随机挑选 size 个
	private List<Actions> pickRandom(Actions[] array, int size) {
		List<Actions> result = new ArrayList<Actions>();

		for (int i = 0; i < array.length; i++) {
			result.add(array[i]);
		}

		for (int j = 0; j < array.length - size; j++) {
			int index = random(iday, j) % result.size();
			result.remove(index);
		}

		return result;
	}

	private List<String> pickRandomStr(String[] array, int size) {
		List<String> result = new ArrayList<String>();

		for (int i = 0; i < array.length; i++) {
			result.add(array[i]);
		}

		for (int j = 0; j < array.length - size; j++) {
			int index = random(iday, j) % result.size();
			result.remove(index);
		}

		return result;
	}

	// 解析占位符并替换成随机内容
	private void parse(Actions item) {
		if (item.name.indexOf("%v") != -1) {
			item.name = item.name.replace("%v", varNames[random(iday, 12)
					% varNames.length]);
		}

		if (item.name.indexOf("%t") != -1) {
			item.name = item.name.replace("%t", tools[random(iday, 11)
					% tools.length]);
		}

		if (item.name.indexOf("%l") != -1) {
			item.name = item.name.replace("%l",
					String.valueOf(random(iday, 12) % 247 + 30));
		}

	}

}
