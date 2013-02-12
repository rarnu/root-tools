package com.rarnu.tools.root.adapter;

import java.util.List;

import com.rarnu.root.pp4.R;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;

public class PartnerAdapter extends InnerAdapter<Object> {

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
