package com.example.testjson;

public class TestArray {

	public int arr = 0;

	public TestArray() {
		
	}
	
	public TestArray(int arr) {
		this.arr = arr;
	}

	@Override
	public String toString() {
		String ret = String.format("{arr:%d}", arr);
		return ret;
	}
}
