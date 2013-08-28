package com.example.testjson;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

public class TestInner {

	public String innerFA = "innerA";
	public String innerFB = "innerB";
	public ArrayList<TestArray> array = new ArrayList<TestArray>();
	public ArrayList<String> arrStr = new ArrayList<String>();
	public HashMap<String, String> map = new HashMap<String, String>();

	public TestInner() {
		for (int i = 0; i < 5; i++) {
			array.add(new TestArray(i));
			arrStr.add("item" + String.valueOf(i));
			map.put(String.format("m%d", i), String.valueOf(i * 2));
		}
	}

	@Override
	public String toString() {
		String ret = String.format("innerFA:%s,innerFB:%s,", innerFA, innerFB);
		ret += "arrStr:[";
		for (int i = 0; i < arrStr.size(); i++) {
			ret += arrStr.get(i) + ",";
		}
		ret += "],";
		ret += "array:[";
		for (int i = 0; i < array.size(); i++) {
			ret += array.get(i).toString() + ",";
		}
		ret += "],";
		ret += "map:[";
		Iterator<String> iter = map.keySet().iterator();
		String key = "";
		while (iter.hasNext()) {
			key = iter.next();
			ret += String.format("{%s:%s},", key, map.get(key).toString());
		}
		ret += "]";
		return ret;
	}
}
