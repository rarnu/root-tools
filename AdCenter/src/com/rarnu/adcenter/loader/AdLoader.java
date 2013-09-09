package com.rarnu.adcenter.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.adcenter.api.AdAPI;
import com.rarnu.adcenter.classes.AdItem;
import com.rarnu.devlib.base.BaseLoader;

public class AdLoader extends BaseLoader<AdItem> {

	private String mac;
	private int page;
	private int pageSize;
	private int type;
	private String extra;
	
	public AdLoader(Context context) {
		super(context);
	}
	
	public void setData(String mac, int page, int pageSize, int type, String extra) {
		this.mac = mac;
		this.page = page;
		this.pageSize = pageSize;
		this.type = type;
		this.extra = extra;
	}

	@Override
	public List<AdItem> loadInBackground() {
		return AdAPI.getAd(mac, page, pageSize, type, extra);
	}

}
