package com.example.testjson;

public class TestMap {
	public int map = 0;

	public TestMap() {

	}

	public TestMap(int map) {
		this.map = map;
	}

	@Override
	public String toString() {
		String ret = String.format("{map=%d}", map);
		return ret;
	}
}
