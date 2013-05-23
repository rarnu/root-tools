package com.rarnu.tools.root.adapter;

import java.util.List;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.R;

public class PartnerAdapter extends BaseAdapter<Object> {

	public PartnerAdapter(Context context, List<Object> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.partner_item, parent, false);
		}
		return v;
	}

	@Override
	public String getValueText(Object item) {
		return "";
	}

}
