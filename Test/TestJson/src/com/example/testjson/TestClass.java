package com.example.testjson;

import java.util.HashMap;
import java.util.Iterator;

public class TestClass {

	public String fa = "a";
	public String fb = "b";
	public TestInner inner = new TestInner();

	public HashMap<String, TestMap> test = new HashMap<String, TestMap>();

	public TestClass() {
		for (int i = 0; i < 10; i++) {
			test.put(String.format("test%d", i), new TestMap(i * 3));
		}
	}

	@Override
	public String toString() {
		String ret = String.format("fa=%s, fb=%s, inner={%s}, ", fa, fb,
				inner.toString());
		ret += "test:[";
		Iterator<String> iter = test.keySet().iterator();
		String key = "";
		while (iter.hasNext()) {
			key = iter.next();
			ret += String.format("{%s:%s},", key, test.get(key).toString());
		}
		ret += "]";
		return ret;
	}
}
