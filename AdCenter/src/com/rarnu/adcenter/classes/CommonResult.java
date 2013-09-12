package com.rarnu.adcenter.classes;

import java.io.Serializable;

import org.json.JSONObject;

public class CommonResult implements Serializable {

	private static final long serialVersionUID = 3287251683696106184L;
	public int result;

	public static CommonResult fromJson(JSONObject json) throws Exception {
		CommonResult item = new CommonResult();
		item.result = json.getInt("result");
		return item;
	}
}
